# Contributing to readboe

`readboe` is an R package for bounded, provenance-bearing access to official
Bank of England statistical data. Contributions are most useful when they keep
the package's public output and provenance contracts explicit, reproducible,
and useful to downstream analysts.

## Before reporting a problem or opening a pull request

Please search the existing [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com)
first. For a proposed change, describe the user problem and the official
source or documented behaviour that motivates it. New behaviour should be
discussed before implementation when it could change a public function,
output columns or classes, dates, errors, cache semantics, or supported source
coverage.

Do not include credentials, access tokens, private data, or a complete local
cache in an issue or pull request. Redact user names and other sensitive paths
from command output where necessary. Use the [security reporting
guidance](SECURITY.md) for a suspected vulnerability rather than a public
issue.

## Local development

The package currently requires R 4.2.0 or later. A minimal development setup
from a clean checkout is:

```r
install.packages(c(
  "digest", "httr2", "readxl", "xml2", "testthat", "openxlsx"
))
```

Run the fixture-based tests without contacting the Bank of England:

```sh
R -q -e 'testthat::test_local()'
R CMD check .
```

The live canaries are opt-in because they depend on the availability and
current shape of official upstream resources. They are not a replacement for
the deterministic fixtures:

```sh
READBOE_RUN_LIVE_TESTS=true R -q -e 'testthat::test_local()'
READBOE_RUN_BANKSTATS_LIVE_TESTS=true R -q -e 'testthat::test_local()'
```

When running live tests, set `READBOE_LIVE_CACHE_DIR` to a disposable cache if
you want to preserve or replay the responses. Do not put credentials in
environment variables or commit downloaded upstream files.

If roxygen comments are changed, regenerate the package documentation and
namespace with roxygen2 before submitting:

```sh
R -q -e 'roxygen2::roxygenise()'
```

Review the resulting `man/` and `NAMESPACE` changes rather than editing those
generated files by hand. Before a pull request, also run `R CMD check --as-cran
.` when the local R installation supports it, and inspect `git diff --check`.

## Implementation expectations

Keep changes small and preserve existing public behaviour unless the change
has been discussed and its contract is documented. In particular, changes
should not silently alter series identity, observation ordering, date
precision, missing or suppression markers, classes, attributes, provenance,
network bounds, cache behaviour, or the package's official-host restrictions.
If a new behaviour is genuinely needed, include fixture-based tests that
specify the output schema and typed failure modes, plus documentation and a
release-note entry where appropriate. Avoid live-network tests as the only
coverage for a parser or transport change.

The package uses typed conditions whose classes begin with `readboe_`, such as
`readboe_config`, `readboe_bounds`, `readboe_timeout`,
`readboe_transport`, `readboe_rate_limit`, `readboe_parse`,
`readboe_schema_drift`, and `readboe_identity_mismatch`. Preserve these
distinctions: invalid caller input, a bounded response, a network failure, and
an upstream format change are different reports and different recovery paths.

Network-facing changes must continue to use HTTPS and the package's
allowlisted Bank of England hosts, bounded requests, and caller-owned
deadlines. Cache changes must remain success-only, integrity checked, and
replayable through `cache_only = TRUE`; they must not write outside the
managed cache or caller-provided temporary locations.

## Branches and pull requests

Create a focused branch from the current default branch and keep unrelated
changes out of it. A useful pull request description includes:

* the user problem and intended contract;
* the exact functions, source surfaces, or documentation affected;
* deterministic tests and checks run, with live-test status called out
  separately;
* any intentional API, output, performance, or dependency impact; and
* documentation, examples, fixtures, and NEWS changes needed for users.

Please make commits easy to review and resolve review feedback with follow-up
commits. Do not rewrite shared history or push to the default branch from a
contributor branch.

## Reporting a reproducible defect

Use the [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com). Include a
small runnable example whenever possible, the exact function arguments, the
official series/table/member or asset URL involved, and the date range. For an
error, report both `class(err)` and `conditionMessage(err)`; for a successful
result, include `names()`, `class()`, and the relevant
`boe_provenance()` fields rather than uploading a large raw response.

Please distinguish a network or rate-limit failure from a parser or schema
failure. State whether the request used the default or a custom `cache_dir`,
whether `cache_only` or `refresh` was set, and whether the same call can be
replayed from a known-good cache. Include the package version or commit, R
version, operating system, and a redacted `sessionInfo()`.

## License

By contributing, you agree that your contribution may be distributed under
the package's existing [MIT license](https://github.com/chartwell-macro/readboe/blob/main/LICENSE).
If your contribution includes
third-party code, data, or assets, identify their license and permissions in
the pull request before adding them.
