"""Coverage guards for the corpus workflow, with no CAS execution."""
import argparse
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

import corpus_shards


class CoverageTests(unittest.TestCase):
    def test_partition_and_merge(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            corpus = root / "corpus"
            corpus.mkdir()
            for name in ("a", "b", "c", "d", "e"):
                (corpus / (name + ".wl")).write_text("x = 1")
                (corpus / (name + ".py")).write_text("x = 1")
            args = argparse.Namespace(corpus=corpus, count=2, shard=0,
                                      directory=root, report=None)
            seen = []

            def evaluate(argv):
                paths = argv[1:argv.index("--backend")]
                seen.extend(paths)
                output = Path(argv[argv.index("--report") + 1])
                output.write_text(json.dumps({"scripts": {
                    str(Path(p).with_suffix("")): {
                        "outcomes": {}, "failures": {}, "oracles": {}}
                    for p in paths}}))
                return 1  # A declared corpus disagreement still retains coverage.

            with patch.object(corpus_shards, "main", side_effect=evaluate):
                for shard in range(2):
                    args.shard = shard
                    args.report = root / f"fortsym-corpus-{shard}.json"
                    corpus_shards.run(args)
            self.assertEqual(len(seen), 5)
            self.assertEqual(len(set(seen)), 5)
            args.report = root / "combined.json"
            corpus_shards.merge(args)
            self.assertEqual(json.loads(args.report.read_text())["summary"]
                             ["scripts"]["total"], 5)
            path = root / "fortsym-corpus-1.json"
            saved = path.read_text()
            path.unlink()
            with self.assertRaisesRegex(ValueError, "expected 2 reports"):
                corpus_shards.merge(args)
            path.write_text((root / "fortsym-corpus-0.json").read_text())
            with self.assertRaisesRegex(ValueError, "duplicate"):
                corpus_shards.merge(args)
            report = json.loads(saved)
            report["scripts"].pop(next(iter(report["scripts"])))
            path.write_text(json.dumps(report))
            with self.assertRaisesRegex(ValueError, "cover the pinned corpus"):
                corpus_shards.merge(args)


if __name__ == "__main__":
    unittest.main()
