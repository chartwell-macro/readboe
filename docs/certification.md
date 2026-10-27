# Package certification record

Status: **readboe 0.1.5 package gate passed; immutable commit recorded at publication**

Audit date: 2026-08-03 (UTC and Australia/Sydney).

## Exact Bankstats identity and history gate

* The official current tables page returned 5,465 valid `EC=` links, 5,261
  unique page identities, and 60 tables. The exact page response SHA-256 is
  `dc059c9c636a7fc775579d87ad1fc3ce19fec169c6e456d8deb844cd95e348d8`.
* Canonical IADB XML validated 5,252 identities. All have non-missing official
  title, frequency, first observation, last observation, categories, source URL,
  and exact metadata-response provenance.
* Nine stale page links are excluded and listed in
  `docs/audits/bankstats-history-2026-08-03/unavailable-page-identities.csv`:
  eight `VPQ*` codes are rejected by the official application as invalid, and
  `PQMBAAW` is explicitly replaced by `PQMBABN` and has no observation bounds.
  No replacement identity is silently substituted.
* The current bulk ZIP still authenticates to
  `ec37bc98fbab6e55916f247732c01cbcdc47e6f978f541a6bbba1387227f52ca`.
  Of the retained identities, 5,008 have an exact current-workbook occurrence
  and 244 are canonical-IADB-only. Workbook mode fails closed for the latter.
* Canonical history begins before the rolling workbook window for all 5,008
  workbook-backed identities. This corrects 4,150 false `2024-07-01` starts.
  The certified catalogue spans 1969-06-30 through 2026-07-29.
* The authenticated catalogue contains 5,252 rows across all 60 linked tables.
  Catalogue SHA-256:
  `fe2497ffabbd7bfaad8040a0bb5bfa77ad62689122eee588fac0911d625481c0`.
  Search-receipt SHA-256:
  `4d3fcb2664b7da76d52202b60bd7081e91109853aaf87102f0305d38a26d88ad`.
  Metadata-receipt SHA-256:
  `d42704aa6673acd1346605814724a5175c22a5829c6f20459dc3a8a26dab9b83`.

## Table-by-table live gate

* One canonical full-history series was fetched for every linked table: 60 of
  60 passed exact series, table, first-date, last-date, and response-hash checks.
* The canaries covered 25,215 observations and 3,119,451 raw response bytes,
  retaining 234 missing observations. No missing row was dropped.
* Exact per-table identity, label, frequency, expected/observed range, row count,
  response bytes, duration, and response SHA-256 are in
  `docs/audits/bankstats-history-2026-08-03/table-live-certification.csv`.
* The full 5,252-series and 60-table reconciliation is in
  `series-history-audit.csv` and `table-history-audit.csv` in the same audit
  directory. Canonical metadata and exact request receipts are retained as RDS
  evidence; intermediate retry checkpoints are not committed.

## Deterministic, build, and live gates

* Installed from source into an isolated library.
* Offline `testthat`: 143 passing expectations across 32 tests; three opt-in
  live tests skipped; zero failures, errors, or warnings.
* Opt-in live suite: all three test groups passed, covering the scoped catalogue,
  rolling time-down and time-across workbooks, full E3.1 history, exact XML/CSV
  non-missing reconciliation, cache-only replay, VUD, five rate metadata IDs,
  Bank Rate, and the latest nominal yield curve.
* `R CMD build`: successful for `readboe_0.1.5.tar.gz`.
* `R CMD check --no-manual`: `Status: OK`; no package code, test,
  documentation, warning, or note failure. Repository-index lookups were
  unavailable in the sandbox, but all declared dependencies were installed and
  the dependency check passed.
* Deterministic coverage includes typed conditions; one absolute caller
  deadline; transient-only retry and Retry-After; hard byte/row/series/archive/
  member/sheet/cell bounds; exact request/cache identity; success-only atomic
  cache; corruption and eviction; offline replay; XML/CSV/VUD schema drift;
  workbook/yield/Bank Rate parsing; NA and duplicate retention; provenance
  save/restore; and no process-global mutation.

## Affirmative upstream limits

* The accepted browse scope is the current Bankstats page, not the wider IADB
  universe and not the discontinued-series workbook.
* Four of the 64 current bulk workbooks (`C3.1`, `C3.4`, `C4.2`, `G1.4`) have no
  exact `EC=` identity on the current tables page. They remain available through
  bounded raw bulk parsing but do not receive invented browse identities.
* The official page and sitemap expose no dated historical bulk-ZIP archive.
  Canonical IADB XML is therefore the full-history authority.
