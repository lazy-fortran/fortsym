import hashlib
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

from scripts.collect_provenance import fetch


class _Response:
    def __init__(self, content):
        self.content = content

    def __enter__(self):
        return self

    def __exit__(self, *_):
        return False

    def read(self):
        return self.content


class ProvenanceCollectionTest(unittest.TestCase):
    def test_fetch_writes_content_and_hash_manifest_record(self):
        content = b"pinned reference\n"
        with tempfile.TemporaryDirectory() as directory:
            destination = Path(directory)
            with patch(
                "scripts.collect_provenance.urlopen",
                return_value=_Response(content),
            ):
                record = fetch("example", "https://example.invalid/ref", destination)

            self.assertEqual((destination / "example.txt").read_bytes(), content)
            self.assertEqual(record["bytes"], len(content))
            self.assertEqual(record["sha256"], hashlib.sha256(content).hexdigest())


if __name__ == "__main__":
    unittest.main()
