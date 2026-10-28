"""Guards for the nightly triage logic, with no GitHub or git access."""
import unittest

import nightly_triage as nt


class TriageTests(unittest.TestCase):
    def test_shard_selection(self):
        self.assertEqual(nt.parse_shards(""), list(range(16)))
        self.assertEqual(nt.parse_shards("5, 3,5"), [3, 5])
        with self.assertRaises(ValueError):
            nt.parse_shards("16")
        self.assertEqual(nt.title_shards("nightly corpus: all shards"), set(range(16)))
        self.assertEqual(nt.title_shards("nightly corpus: shards 3,5"), {3, 5})
        self.assertIsNone(nt.title_shards("corpus"))

    def test_failing_shards_from_jobs(self):
        jobs = [{"name": "Corpus shard 3", "conclusion": "failure"},
                {"name": "Corpus shard 5", "conclusion": "success"},
                {"name": "Corpus shard 12", "conclusion": "timed_out"},
                {"name": "Plan shards", "conclusion": "failure"}]
        self.assertEqual(nt.failing_shards(jobs), [3, 12])

    def test_last_green_needs_covering_nightly(self):
        nightly = [
            {"databaseId": 9, "conclusion": None, "displayTitle": "nightly corpus: all shards"},
            {"databaseId": 8, "conclusion": "success", "headSha": "partial",
             "displayTitle": "nightly corpus: shards 5"},
            {"databaseId": 7, "conclusion": "failure", "headSha": "red",
             "displayTitle": "nightly corpus: all shards"},
            {"databaseId": 6, "conclusion": "success", "headSha": "green",
             "displayTitle": "nightly corpus: all shards"}]
        corpus = [{"conclusion": "success", "event": "push", "headSha": "push"}]
        self.assertEqual(nt.last_green(nightly, corpus, [3], 9)["headSha"], "green")
        self.assertEqual(nt.last_green(nightly[:3], corpus, [3], 9)["headSha"], "push")
        sample = [{"conclusion": "success", "event": "merge_group", "headSha": "q"}]
        self.assertIsNone(nt.last_green(nightly[:3], sample, [3], 9))

    def test_report_and_closing(self):
        body = nt.failure_report(
            branch="main", bad="b" * 40, good="a" * 40, commits=3,
            culprit={"sha": "c" * 40, "subject": "Break shard 3"}, pull=42,
            shards=[3, 12], run_url="https://x/run", job_urls={3: "https://x/j3"},
            server="https://github.com", repo="o/r")
        self.assertIn("[3](https://x/j3), 12", body)
        self.assertIn("First bad commit: " + "c" * 40 + " Break shard 3 from #42", body)
        self.assertIn("compare/" + "a" * 40 + "..." + "b" * 40, body)
        self.assertNotIn("@", body)
        recorded = nt.issue_shards([body, "a comment"])
        self.assertEqual(recorded, {3, 12})
        self.assertFalse(nt.should_close([3], recorded))
        self.assertTrue(nt.should_close(range(16), recorded))
        self.assertFalse(nt.should_close(range(16), set()))


if __name__ == "__main__":
    unittest.main()
