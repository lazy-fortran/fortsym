#!/usr/bin/env python3
"""Triage a failed nightly corpus run: bisect the failing shards and file an issue.

Subcommands, all run inside GitHub Actions with GH_TOKEN set:

  plan      turn the dispatch input into the shard list
  failure   find the failing shards and the last green run, bisect, report
  step      one `git bisect run` step: build, then run the given shards
  success   close the open failure issue once its shards are green again
"""
import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

SHARD_COUNT = 16
LABEL = "nightly-failure"
MARKER = re.compile(r"<!-- nightly-failing-shards: ([0-9,]*) -->")
SHARD_JOB = re.compile(r"^corpus / Corpus shard (\d+)$|^Corpus shard (\d+)$")


def parse_shards(text):
    """Shard list from a dispatch input; empty means every shard."""
    text = (text or "").strip()
    if not text or text == "all":
        return list(range(SHARD_COUNT))
    shards = sorted({int(item) for item in text.split(",") if item.strip()})
    if not shards or shards[0] < 0 or shards[-1] >= SHARD_COUNT:
        raise ValueError(f"shards must lie in 0..{SHARD_COUNT - 1}: {text!r}")
    return shards


def title_shards(title):
    """Shards a run covered, from its run title; None if not a nightly title."""
    if title == "nightly corpus: all shards":
        return set(range(SHARD_COUNT))
    match = re.fullmatch(r"nightly corpus: shards (.+)", title or "")
    if not match:
        return None
    try:
        return set(parse_shards(match.group(1)))
    except ValueError:
        return None


def failing_shards(jobs):
    shards = set()
    for job in jobs:
        match = SHARD_JOB.match(job["name"])
        if match and job.get("conclusion") in ("failure", "timed_out", "cancelled"):
            shards.add(int(match.group(1) or match.group(2)))
    return sorted(shards)


def last_green(nightly_runs, corpus_runs, needed, current_run):
    """Newest green run covering the needed shards.

    Nightly runs are preferred; before the first green nightly, a full corpus
    run of corpus.yml on the same branch counts, since it ran every shard.
    """
    for run in nightly_runs:
        if run["databaseId"] == current_run or run.get("conclusion") != "success":
            continue
        covered = title_shards(run.get("displayTitle"))
        if covered is not None and set(needed) <= covered:
            return run
    for run in corpus_runs:
        if (run.get("conclusion") == "success"
                and run.get("event") in ("push", "release", "workflow_dispatch")):
            return run
    return None


def issue_title(branch):
    return f"Nightly corpus failure on {branch}"


def issue_shards(texts):
    """All failing shards recorded in an issue body and its comments."""
    shards = set()
    for text in texts:
        for match in MARKER.finditer(text or ""):
            shards.update(int(s) for s in match.group(1).split(",") if s)
    return shards


def failure_report(*, branch, bad, good, commits, culprit, pull, shards,
                   run_url, job_urls, server, repo, note=None):
    lines = [f"The nightly corpus failed on `{branch}` at {bad}.", ""]
    lines.append("Failing shards: " + ", ".join(
        f"[{s}]({job_urls[s]})" if s in job_urls else str(s) for s in shards))
    if good:
        lines.append(f"Commit range: {good}..{bad} ({commits} commits since the "
                     f"last green run, [compare]({server}/{repo}/compare/{good}...{bad}))")
    else:
        lines.append("Commit range: unknown, no earlier green run covers these shards")
    if culprit:
        subject = culprit["subject"]
        via = f" from #{pull}" if pull else ""
        lines.append(f"First bad commit: {culprit['sha']} {subject}{via}")
    if note:
        lines.append(note)
    lines.append(f"Run and bisection log: {run_url}")
    lines.append("")
    lines.append(f"<!-- nightly-failing-shards: {','.join(map(str, shards))} -->")
    return "\n".join(lines)


def should_close(green_shards, recorded_shards):
    return bool(recorded_shards) and set(recorded_shards) <= set(green_shards)


# ---- GitHub and git plumbing -------------------------------------------------

def gh(*args, json_output=True):
    out = subprocess.run(["gh", *args], check=True, capture_output=True,
                         text=True).stdout
    return json.loads(out) if json_output and out.strip() else out


def git(*args, check=True):
    return subprocess.run(["git", *args], check=check, capture_output=True,
                          text=True).stdout.strip()


def env(name):
    return os.environ[name]


def open_issue(repo, branch):
    issues = gh("issue", "list", "--repo", repo, "--label", LABEL, "--state",
                "open", "--limit", "50", "--json", "number,title")
    for issue in issues:
        if issue["title"] == issue_title(branch):
            return issue["number"]
    return None


def cmd_plan(args):
    shards = parse_shards(args.shards)
    with open(env("GITHUB_OUTPUT"), "a") as out:
        out.write(f"shards={json.dumps(shards)}\n")
        out.write(f"all={'true' if len(shards) == SHARD_COUNT else 'false'}\n")


def cmd_step(args):
    """Exit 0 for good, 1 for bad; a build failure counts as bad."""
    build = ["nix", "develop", "--command", "cmake"]
    configure = subprocess.run(build + ["-S", ".", "-B", "build", "-G", "Ninja",
                                        "-DCMAKE_BUILD_TYPE=Release",
                                        "-DFORTSYM_BUILD_TESTING=OFF"])
    if configure.returncode != 0:
        return 1
    jobs = os.environ.get("SCI_CPUS", "8")
    if subprocess.run(build + ["--build", "build", "--target", "fortsym_wl_run",
                               "-j", jobs]).returncode != 0:
        return 1
    workspace = Path.cwd()
    for shard in parse_shards(args.shards):
        report = Path(args.reports) / f"bisect-{git('rev-parse', '--short', 'HEAD')}-{shard}.json"
        status = subprocess.run([
            "nix", "develop", "--command", "env",
            f"PATH={workspace / 'build' / 'bin'}:{os.environ['PATH']}",
            "python", "scripts/corpus_shards.py", "run", "--shard", str(shard),
            "--report", str(report)]).returncode
        if status != 0:
            return 1
    return 0


def bisect(good, bad, shards, reports):
    """First bad commit in good..bad for the given shards.

    Returns None when the shards also fail at the green commit, since then
    no commit in the range introduced the failure.
    """
    script = Path(os.environ.get("RUNNER_TEMP", "/tmp")) / "nightly_triage.py"
    shutil.copy(__file__, script)  # bisect checks out older versions of this file
    step = [sys.executable, str(script), "step",
            "--shards", ",".join(map(str, shards)), "--reports", reports]
    git("checkout", "--quiet", good)
    try:
        good_still_passes = subprocess.run(step).returncode == 0
    finally:
        git("checkout", "--quiet", bad)
    if not good_still_passes:
        return None
    git("bisect", "start", bad, good)
    try:
        subprocess.run(["git", "bisect", "run", *step], check=True)
        sha = git("rev-parse", "refs/bisect/bad")
    finally:
        print(git("bisect", "log", check=False))
        git("bisect", "reset", check=False)
    return {"sha": sha, "subject": git("log", "-1", "--format=%s", sha)}


def cmd_failure(args):
    repo, run_id = env("GITHUB_REPOSITORY"), int(env("GITHUB_RUN_ID"))
    server, branch, bad = env("GITHUB_SERVER_URL"), env("GITHUB_REF_NAME"), env("GITHUB_SHA")
    jobs = gh("api", "--paginate", f"repos/{repo}/actions/runs/{run_id}/jobs",
              "--jq", ".jobs[]", json_output=False)
    jobs = [json.loads(line) for line in jobs.splitlines() if line.strip()]
    shards = failing_shards(jobs)
    if not shards:
        raise SystemExit("the corpus failed but no shard job did")
    job_urls = {}
    for job in jobs:
        match = SHARD_JOB.match(job["name"])
        if match:
            job_urls[int(match.group(1) or match.group(2))] = job["html_url"]
    fields = "databaseId,headSha,conclusion,displayTitle,event"
    nightly = gh("run", "list", "--repo", repo, "--workflow", "nightly.yml",
                 "--branch", branch, "--limit", "100", "--json", fields)
    corpus = gh("run", "list", "--repo", repo, "--workflow", "corpus.yml",
                "--branch", branch, "--limit", "100", "--json", fields)
    green = last_green(nightly, corpus, shards, run_id)
    good = green["headSha"] if green else None
    culprit, pull, note, commits = None, None, None, 0
    if good:
        commits = int(git("rev-list", "--count", f"{good}..{bad}"))
        if good == bad:
            note = "No commit since the last green run: the failure is not from a code change."
        elif subprocess.run(["git", "merge-base", "--is-ancestor", good, bad]).returncode:
            note = "The last green commit is not an ancestor; bisection skipped."
        else:
            culprit = bisect(good, bad, shards, args.reports)
            if culprit:
                pulls = gh("api", f"repos/{repo}/commits/{culprit['sha']}/pulls")
                pull = pulls[0]["number"] if pulls else None
            else:
                note = ("The failing shards also fail at the last green commit when "
                        "rerun, so the failure is not from a commit in the range.")
    body = failure_report(
        branch=branch, bad=bad, good=good, commits=commits, culprit=culprit,
        pull=pull, shards=shards, run_url=f"{server}/{repo}/actions/runs/{run_id}",
        job_urls=job_urls, server=server, repo=repo, note=note)
    print(body)
    number = open_issue(repo, branch)
    if number:
        gh("issue", "comment", str(number), "--repo", repo, "--body", body,
           json_output=False)
    else:
        gh("label", "create", LABEL, "--repo", repo, "--color", "B60205",
           "--description", "The nightly full corpus failed", "--force",
           json_output=False)
        gh("issue", "create", "--repo", repo, "--title", issue_title(branch),
           "--label", LABEL, "--body", body, json_output=False)


def cmd_success(args):
    repo, branch = env("GITHUB_REPOSITORY"), env("GITHUB_REF_NAME")
    number = open_issue(repo, branch)
    if not number:
        return
    issue = gh("issue", "view", str(number), "--repo", repo, "--json", "body,comments")
    recorded = issue_shards([issue["body"]] + [c["body"] for c in issue["comments"]])
    green = parse_shards(args.shards)
    if not should_close(green, recorded):
        print(f"issue #{number} records shards {sorted(recorded)}; this run covered {green}")
        return
    run_url = f"{env('GITHUB_SERVER_URL')}/{repo}/actions/runs/{env('GITHUB_RUN_ID')}"
    gh("issue", "close", str(number), "--repo", repo, "--comment",
       f"The nightly corpus is green again at {env('GITHUB_SHA')}: {run_url}",
       json_output=False)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("action", choices=["plan", "failure", "step", "success"])
    parser.add_argument("--shards", default="")
    parser.add_argument("--reports", default=os.environ.get("RUNNER_TEMP", "/tmp"))
    args = parser.parse_args(argv)
    return {"plan": cmd_plan, "failure": cmd_failure, "step": cmd_step,
            "success": cmd_success}[args.action](args) or 0


if __name__ == "__main__":
    sys.exit(main())
