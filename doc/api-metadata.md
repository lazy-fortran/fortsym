# Pinned compatibility metadata

The `sympy-api*.json.gz` files retain the full SymPy 1.14.0 API inventory,
classification, naming audit, baseline and difference report. Compression
preserves every JSON record; the release-profile and compatibility-profile
checks decompress them automatically, using only Python's standard library.
Running those checks does not require SymPy.

Run the commands below from the checkout root with Python 3.11 or newer. For
inspection or use with the individual inventory tools, expand the files:

```bash
python3 scripts/api_metadata.py unpack
```

The command checks the expanded bytes against `api-metadata.sha256`. Expanded
JSON files are ignored by Git. The profile checkers always prefer the tracked
compressed files over expanded copies, which may be stale after an update.

To refresh the inventory, use an isolated environment with SymPy 1.14.0:

```bash
python3 scripts/api_metadata.py unpack
python3 scripts/inventory_sympy_api.py doc/sympy-api-inventory.json --version 1.14.0
python3 scripts/classify_sympy_api.py doc/sympy-api-inventory.json doc/sympy-api-classification.json
python3 scripts/audit_api_naming.py doc/sympy-api-classification.json doc/sympy-api-naming-audit.json
python3 scripts/diff_sympy_api.py diff doc/sympy-api-baseline-1.14.0.json doc/sympy-api-inventory.json doc/sympy-api-diff-1.14.0.json --fail-on-diff
python3 scripts/api_metadata.py pack
python3 scripts/check_release_profile.py doc/release-profile.toml --metadata-only
```

The first command retains the original baseline. Review changes to the
inventory, naming policy, difference ledger and supported surface together;
an unexpected API difference is a failing check. `pack` writes deterministic
gzip streams without filenames or timestamps and refreshes the checksum
manifest. It does not create a new baseline or approve a compatibility change.
