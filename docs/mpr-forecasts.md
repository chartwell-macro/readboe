# Forecast reports: saved reads, maintenance and source replay

`read_boe_mpr_forecasts()` reads a compact authenticated numeric snapshot of the
official Inflation Report, Monetary Policy Report and joint MPR/Financial
Stability Report lineage. Ordinary release, asset, catalogue and observation
calls perform no HTTP, subprocess execution, PDF extraction or raw source reads.
They work with Poppler absent and reuse the loaded observation table. The
installed baseline is read-only; a validated local refresh takes precedence.

Explicit source discovery unions the
Bank's two sitemaps with its archive search results, including reports that appear
in search before a sitemap refresh. It uses the dates attached to each report,
not sidebar news dates, a release allowlist or an assumed quarterly calendar.

```r
editions <- boe_mpr_releases(include_upcoming = TRUE)
assets <- boe_mpr_assets("2020-05")
paths <- boe_mpr_catalogue("2026-07", include_excluded = TRUE)

x <- read_boe_mpr_forecasts(
  "2026-07", measure = "cpi_inflation", frequency = "Q",
  scenario = c("central", "milder", "adverse", "central_memo"),
  deadline = boe_deadline(1)
)
p <- boe_provenance(x)
replay <- read_boe_mpr_forecasts(
  "2026-07", series_id = unique(x$series_id),
  expected_sha256 = p$raw_sha256
)
```

`latest` selects the newest saved published edition; `all` combines numeric
outcomes, and an exact absent edition raises its stored evidence-bearing
condition. Scheduled placeholders
are returned only with `include_upcoming = TRUE` and cannot supply forecasts.
The rediscovered baseline contains 140 editions: 135 published reports
from February 1993 to 30 July 2026, and five future placeholders. In the early
archive, a full year's dates are all the first of the report month. These cohorts
have `release_date_precision = "month"` and `release_date = NA`; the original
website date and month start remain separately available. An exact day is not
invented. The structured bundle contains 149,526 observations including past
targets, of which 53,873 are publication-quarter/year and later observations
with distributions enabled. There are 115 numeric outcomes and 20 affirmative
chart/prose-only outcomes. Numeric data and receipts compress to about 2.2 MiB;
no source PDF, HTML, XLS/XLSX or ZIP is shipped in the forecast bundle.
Gzip was selected over the smaller XZ representation to keep cold reads within
the one-second target with 25% corpus headroom; no numeric or provenance fields
were removed.

## Source formats and coverage

The reader recognizes publication-vintage matrices, labelled scenario columns,
current fan workbooks (including binary XLS), grouped historical parameters,
transposed market conditioning profiles and GDP levels with separately labelled
backcast variants. It also reads printed MPC probability bins, market interest
rate conditioning tables, annual mode/median/mean summaries and indicative annual
profiles. Profile labels, rows and footnotes can continue across PDF pages.
Poppler's `pdftotext` must be available on PATH only when explicitly rebuilding
report PDF tables; no extra R package is needed. Missing Poppler in that route
raises `readboe_dependency` with the capability and dependency named.

An older edition's own numeric sources take precedence. By default a later
*discovered official historical workbook* can fill missing paths only when an
exact original vintage row exists. These rows retain the later `source_edition`,
URL, workbook member, original vintage label and source cells. Original and
historical overlapping representations remain in provenance. Use
`historical_source = FALSE` to require the edition's own assets. Enabling optional
distributions does not remove scalar paths supplied by historical fallback.

The original report establishes interpretation. May 2020's illustrative scenario
remains illustrative when supplemented by later historical rows. A central label
in a general databank cannot override that report context. Market interest-rate
paths are `conditioning_assumption`, not predictions of policy decisions.
Constant-rate paths, market-conditioned paths, GDP backcast variants, dated
comparators and named scenario memos have separate identities.

The source audit records an outcome for every published edition, including
chart/prose-only reports. `readboe_no_numeric_forecast` carries inspected-source
receipts, page evidence and the historical-vintage search result. An edition with
only probability tables raises `readboe_no_scalar_forecast` under the default
selection; use `include_distributions = TRUE` to read those tables. Source,
transport, dependency and unrecognized-format failures are separate typed errors.
Absence is never inferred merely from a failed parser.

General chart workbooks, outside-forecaster surveys, outturn series, short-term
component tables and policy-rule simulations are inventoried as supporting
material. They are not silently relabelled as the Bank's forecast paths. In
particular, visual-summary Bank Rate sheets contain daily historical outturns,
including very large formatted ranges; the dedicated market profile supplies
quarterly conditioning paths. No chart point, scenario weight or unpublished fan
quantile is reconstructed.

## Observations and selection

Selectors `measure`, `scenario`, `statistic`, `frequency` and `kind` use exact
catalogue values. `series_id` selects complete path IDs instead and cannot be
combined with dimensional selectors. Unavailable combinations raise
`readboe_no_data`. The observation key is `series_id` plus `target_period`.

Rows preserve original units and definitions, release and forecast vintages,
source vintage labels, source edition, raw values and workbook coordinates or
physical PDF page and line/geometry coordinates. Annual averages, annual changes
and Q4 endpoints have distinct statistics. `frequency = "A"` denotes a yearly
sampled table; a Q4 endpoint still has a target such as `2027-Q4` and representative
period-start date `2027-10-01`. Annual averages have target `2027` and date
`2027-01-01`. Dates are not exact observation days.

The default keeps the path's publication quarter/year and later targets.
`include_history = TRUE` retains earlier targets from that same published path.
No outturn/estimate status is inferred from dates or formatting. Missing cells
remain explicit observations with their original marker and `value = NA`.
Unknown nonnumeric tokens fail closed.

`include_distributions = TRUE` adds explicitly printed probability rows,
uncertainty and skewness parameters, and explicitly printed percentiles. Published
quantile rates retain a separate `quantile_probability` and source label; current
and parenthesised comparator conditioning are scoped to their own named vintages.
Mean and median scalar paths are preserved
where published. Censored probabilities such as `<1`, `<5%` and `>95%` retain
`value = NA`, `value_status = "censored"`, original token and an upper/lower bound
with its inclusive flag. Workbook probability fractions and PDF percentage units
remain distinct. Inequality directions and events are part of the path identity.

The original 2026 defaults remain 165 February, 455 April and 438 July observations.
April supplies scenarios A/B/C and a February comparator, with seven explicit
`n.a.` endpoints; it has no new central path. July adds milder/adverse and central
memos. Its numerical matrices label the final row August, while the report is
July. The reader reconciles that anomaly by exact agreement with independent
named central memo paths for multiple measures and horizons, not by a month
exception or a nearest-date rule. Original August labels remain in the result.
The official July CPI milder value for 2027 Q1 is **2.9**, in sheet 44 cell B122;
3.0 is the previous quarter's B121.

## Bounds and provenance

Defaults bound each asset to 40 MiB, outer archives to 200 members and 200 MiB
uncompressed, and each selected workbook to 150 sheets, 10,000 rows, 512 columns,
two million rectangular cells and 200,000 output rows. XLSX internal ZIPs have a
separate 1,000-member bound. Cell references, declared dimensions and duplicate
coordinates are checked before allocation. Generic release-reader defaults are
unchanged. PDF extraction bounds input, output text, pages and subprocess time.

Unsafe, duplicate or noncanonical ZIP paths are rejected. Extraction streams
selected members into fresh regular files and never applies archived symlink
attributes. One absolute caller deadline spans discovery, acquisition and parsing,
including cache hits. Bounded native operations are checked on return; an expired
result is not returned.

Provenance includes SHA-256 revision receipts for source assets and parsed workbook
members, original/final URLs, retrieval metadata, archive/table inventory,
alternate representations, parser version `mpr-layouts-v2`, classifications,
rights links and transforms. `expected_sha256` pins the primary numeric asset;
all supplemental receipts must also be retained to identify a multi-source result
completely. Refreshing corrected source bytes produces new receipts. Saved
structured observations retain these receipts; they are distinct from the
bounded raw cache used for explicit reconstruction. Search POST cache keys include their
complete canonical form; legacy GET cache identities are unchanged.

## Local updates and scheduling

Set `READBOE_MPR_DATA_DIR` or pass `data_dir` consistently to the updater and
readers. The default is `file.path(tools::R_user_dir("readboe", "data"), "mpr")`.
Reads never create this directory or update the installed bundle. With no local
generation, they use the bundled baseline.

```r
boe_mpr_update(data_dir = "my-forecasts", deadline = 3600)
boe_mpr_snapshot_status(data_dir = "my-forecasts")
read_boe_mpr_forecasts("latest", data_dir = "my-forecasts")
# Force a same-day unconditional source check, or explicitly rebuild all parsers:
boe_mpr_update(data_dir = "my-forecasts", force = TRUE)
boe_mpr_update(data_dir = "my-forecasts", rebuild = TRUE)
```

New numeric releases and revisions do not require a new package installation.
For automatic operation, call the calendar-aware maintenance API from an
external hourly runner using a durable data directory:

```r
boe_mpr_calendar()  # Official MPR dates, explicit clocks and source receipts
boe_mpr_maintain(data_dir = "my-forecasts", deadline = 3600)
boe_mpr_maintenance_status(data_dir = "my-forecasts")
```

The installed script exposes the same operation and a read-only status mode:

```sh
Rscript /path/to/library/readboe/scripts/update-mpr-forecasts.R --maintain --json --data-dir /path/to/forecasts --deadline 3600
Rscript /path/to/library/readboe/scripts/update-mpr-forecasts.R --maintenance-status --json --data-dir /path/to/forecasts
```

Locate that entry point with
`system.file("scripts", "update-mpr-forecasts.R", package = "readboe")`. All
modes use the same default directory as readers. The package does not install or
activate an OS schedule. Its ordinary readers and startup never invoke either
maintenance mode.

`boe_mpr_maintain()` refreshes the
[official MPC calendar](https://www.bankofengland.co.uk/monetary-policy/upcoming-mpc-dates)
and [publication listing](https://www.bankofengland.co.uk/news/latest-and-upcoming)
daily. Only table rows linked to an MPR supply dates; minutes-only meetings are
excluded. Explicit clocks use `Europe/London`, including daylight-saving
changes, and are reported in UTC. A listed date without a publication clock
retains an unknown `next_release_at`. Local midnight then starts labelled
metadata polling; it is not asserted to be a publication time. Dates are
rediscovered across years and after release ingestion. A missed edition can
survive a calendar year rollover for catch-up; removed dates within a covered
year are closed.

Hourly invocations before `next_due_at` read only small maintenance metadata:
they perform no HTTP, numeric-table load, subprocess or write. Due work includes
an independent daily check of historical sources, probes of a due report's
publication heading and links, and bounded retries. Unpublished placeholders
remain scheduled even when their advertised day has passed, so a late report
cannot block historical revision checks or cause unreleased assets to be parsed.
Metadata probes use a 15-minute minimum interval within the default 48-hour
release window, then six-hour intervals while a missing report remains late.
An hourly runner naturally checks at its first wake after each due time.

An available new publication, changed asset links or a corrected asset validator
can trigger a same-day update with conditional HTTP checks. These triggers do
not force unconditional archive downloads. The same signal is retried no sooner
than six hours, with at most four release-triggered full checks per rolling
24 hours. Calendar/probe errors back off from 15 minutes to six hours while
keeping the last good calendar. Full-update failures keep the last good numeric
generation and retry after six hours. Daily historical checks remain independent
of the release budget. `force = TRUE` on maintenance refreshes the calendar and
evaluates due work; the direct updater's `force` separately means unconditional
source retrieval.

The atomic schema-1 `MAINTENANCE.rds` and `MAINTENANCE.previous.rds` files store
calendar receipts, known editions, per-release fingerprints, retry gates and a
bounded history of 32 closed release records. A separate maintenance lock
serializes state changes; numeric writes retain their own existing writer lock.
An attempt journal is written before expensive work, so a hard process exit
keeps its retry budget. Recovery recognizes a numeric commit that already
completed and otherwise preserves the interrupted attempt's retry gate. A
corrupt state falls back to its previous valid copy and is repaired on the next
maintenance run. Neither state recovery nor a failed check changes last-good
numeric data.

JSON/status fields include `schema`, `status` (`success`, `unchanged`, `idle`,
`waiting` or `failed`), `outcome`, `next_due_at`, `next_release_edition`,
`next_release_date`, `next_release_at`, `next_release_time_known`,
`next_release_poll_at`, calendar freshness/horizon fields, revision next-check
and successful-check times, the snapshot hash, overdue indicators and `errors`.
Times are UTC ISO8601 strings. An unavailable or uninitialized calendar has a null
`calendar_horizon_exhausted`; only a known calendar with no remaining release
is exhausted. `next_due_at` is the earliest calendar, revision,
release or retry event, so it will usually precede the next report publication.
Status reads do not run due work. Failed CLI checks exit nonzero.

The source-controlled `.github/workflows/check-mpr-forecasts.yml` uses the direct
updater mode daily at 05:17 UTC and on manual dispatch, with a disposable
runner-local directory. It checks sources and reports metadata only; it does
not upload numeric data, commit a bundle, or distribute updates to consumers.
Calendar-aware hourly operation requires a durable consumer directory and an
external runner calling `--maintain` as shown above.

An update rediscovers the full archive and checks every report page and relevant
numeric/PDF asset, including old editions and unchanged URLs. It uses ETag or
Last-Modified validators when available; otherwise it retrieves and hashes the
bytes. `force = TRUE` retrieves unconditionally. A 304 retains its known revision
hash and cannot create an empty raw cache entry. Changed receipts, dates, links
and parser fingerprints determine reparsing. New or revised historical databanks
conservatively invalidate earlier outcomes, including affirmative numeric
absences and earlier searches with no match. Existing matches never establish
that a future databank cannot add a path.
A parser change or `rebuild = TRUE` bypasses the same-day throttle. Ordinary
attempts, including failures, run at most once per UTC day; `force` permits a
retry. `failed_throttled` is not a successful freshness result.

Candidates must retain the complete discovered published inventory, unique
identities/targets, valid dates and units, matching source hashes and provenance,
and all previously available identities/targets. An intentional removal requires
explicit `allow_coverage_loss = TRUE` after review. Network, extraction, schema,
deadline or validation failure commits no candidate and does not advance the
last successful check. A single atomic lock excludes concurrent writers and can
recover an abandoned same-host process lock after 60 seconds. It does not break
a live writer's lock, including with `force`.

`CURRENT.rds` commits references to immutable hash-named generations only after
staging and authenticating the complete candidate. The generation's successful
status and check time are inside that same atomic pointer commit. Standalone
`STATUS.rds` records no-op checks and failed/interrupted attempts; readers accept
it only for the data generation they actually selected. Earlier uncommitted or
mismatched status cannot advance freshness. A graceful interrupt is recorded and
propagated. A process crash before the pointer rename leaves the prior data,
hash and successful-check time intact. After the commit, even an interrupted
caller sees the fully committed generation and matching status.

Readers use the current
generation, then a validated previous generation, then the bundle. A failed
attempt or corrupt refreshed generation is visible through snapshot provenance,
`boe_mpr_snapshot_status()`, and a warning on the next ordinary read. Numeric
no-ops write check/source metadata only. At most current, previous and two recent
unreferenced numeric generations are retained; old staged temporary files are
removed after a day. A reader retries a changed pointer if pruning races its read.

Updater source bodies use `data_dir/raw` as a disposable working cache by default.
Those SHA-named RDS containers hold actual HTML, PDF, XLS/XLSX or ZIP response
bodies, not parsed tables. Acquisition may temporarily need substantial disk
space: a complete archive check has used about 1 GiB. The working cache is bounded
to 1,000 entries and 2 GiB by default, plus an in-progress atomic write. Success,
failure and graceful interruption remove owned response bodies and cache-write
temporaries before releasing the writer lock. A hard kill can leave bounded
working files; the next recovered writer clears them, including on a throttled
retry. Unrelated files in the directory are preserved. Idle maintenance and saved
reads never touch the working cache.

Parsed numeric generations, `SOURCES.rds` validators/hashes, structured link
inventories, provenance and status remain after cleanup. Unchanged conditional
checks reuse that metadata and require no parsing or retained source bodies. If
an affected edition needs a body that is no longer present, the updater fetches
it and verifies that its hash still matches the checked revision before parsing.
It does not reparse every edition because the working cache was cleared. Release
metadata probes hash their responses in memory and cannot write or clear the
updater's working cache.

Use `retain_raw = TRUE` on `boe_mpr_update()` or `boe_mpr_maintain()`, or add CLI
`--retain-raw`, to keep the bounded source cache after an update. Retained entries
are pruned after seven days without a rewrite by default; a fresh HTTP 200
rewrites the entry even when its hash is unchanged. The direct updater's
`raw_max_bytes` and `max_raw_age_days` configure the byte bound and retained age.
Both require positive values; tiny byte budgets can evict bodies needed during
parsing and cause an explicit update failure. Normal saved reads remain
available, and cleanup never removes the installed baseline or changes saved
numeric results. Explicit `source = "raw"` reconstruction keeps its separate
caller-controlled cache behavior.

Use `source = "raw"` on release/asset/catalogue/read calls, or the exported
`read_boe_mpr_forecasts_raw()`, for source reproduction with the existing raw
cache/deadline/bounds contracts. `refresh = TRUE` is rejected on saved reads to
prevent accidental network maintenance in selection code. `cache_only` is a raw
transport option; saved reads are always offline. Source hash pins still check
the primary numeric revision. Inspect snapshot and supplemental source receipts
to identify every revision used by a multi-source result.

After a successful full local rebuild, maintainers can run
`Rscript data-raw/build-mpr-snapshot.R DATA_DIR OUTPUT_DIRECTORY` to prepare a new
bundle for review. It does not publish the result or resolve redistribution
rights.

## Verification and reuse

Default tests use synthetic HTML, cells, ZIPs and small XLSX files without network
access. Set `READBOE_MPR_ARCHIVE_CACHE` to a hash-checked raw cache for official
source replay and format-transition anchors. Set `READBOE_RUN_MPR_LIVE_TESTS=true`
for live archive discovery, a fresh latest-source acquisition and cache replay.
The saved-data suite additionally forbids network/raw/PDF calls, checks the full
offline bundle, and exercises source revisions, new editions, parser changes,
changed links, failed/aborted commits, corruption, throttling and concurrent
readers/writers. The maintenance suite checks calendar row scoping, winter and
summer UTC conversion, unknown clocks, year rollover, cancellations, late
reports, rolling retry budgets, metadata-only idle calls, lock contention and
actual child-process crash recovery. Optional `READBOE_MPR_CALENDAR_FIXTURE` and
`READBOE_MPR_TIMES_FIXTURE` paths enable replay of retained official HTTP entries;
raw calendar HTML is not bundled. The separate full-corpus audit records every discovered published edition; a few
selected regression editions do not establish archive completeness.

```r
pkgload::load_all()
testthat::test_local()
```

Official sources: [Inflation Report archive](https://www.bankofengland.co.uk/inflation-report/inflation-reports),
[MPR archive](https://www.bankofengland.co.uk/monetary-policy-report/monetary-policy-report),
[May 2020 joint report](https://www.bankofengland.co.uk/report/2020/monetary-policy-report-financial-stability-report-may-2020),
[July 2026 report](https://www.bankofengland.co.uk/monetary-policy-report/2026/july-2026).
Only pre-extracted numeric values and structured provenance are bundled. The
[Bank's legal terms](https://www.bankofengland.co.uk/legal) distinguish Database OGL
data from website Resources and third-party materials. Applicable reuse permission
and report/market-data notices remain a downstream redistribution gate; this
implementation does not establish commercial redistribution permission.
