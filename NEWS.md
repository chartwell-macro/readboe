# readboe 0.2.0

* Uses a read-only Windows process listing when checking writer-lock owners;
  process signalling is reserved for POSIX liveness checks. Active or unknown
  owners keep their locks, and confirmed abandoned locks remain recoverable.

* Updates and maintenance now discard working source bodies by default while
  retaining numeric snapshots, validators and provenance. Cleanup runs under the
  writer lock on success, failure and graceful interrupt; recovered writers clear
  bounded hard-kill leftovers. `retain_raw = TRUE` or CLI `--retain-raw` opts into
  the existing bounded cache. Release probes hash responses without writing bodies.
* Adds `boe_mpr_calendar()`, `boe_mpr_maintain()` and metadata-only
  `boe_mpr_maintenance_status()`, with installed CLI JSON modes. Official dates
  and explicit publication clocks determine the next due event in UTC using
  London daylight-saving rules. Persistent maintenance state keeps daily
  historical checks independent of late releases, bounds release retries, and
  recovers interrupted work. No schedule is installed or activated by the package.
* Commits successful generation status with the atomic snapshot pointer. Actual
  interrupts and process crashes before commit preserve both last-good values
  and freshness; advisory status from another generation is ignored.
* Ordinary forecast releases, catalogues and selections now use an authenticated
  bundled numeric snapshot, with no HTTP, raw-file reads or PDF subprocesses.
  It retains the complete published corpus, history, distributions, provenance
  and explicit numeric absences. Loaded tables are reused in memory.
* Adds `boe_mpr_update()` and an installed scheduler entry point to check new
  reports, changed links and same-URL revisions. Validated immutable generations
  feed default reads from a configurable user data directory; failures retain
  the last good snapshot and its successful-check time. Daily repository checks
  use the same entry point without distributing refreshed data.
* Keeps explicit raw reconstruction with `read_boe_mpr_forecasts_raw()` or
  `source = "raw"`. Poppler is required only for raw PDF work and updates that
  need PDF extraction.
* Adds live full-lineage Inflation Report/MPR discovery, exact asset discovery,
  source-derived forecast catalogues and `read_boe_mpr_forecasts()`.
* Recognizes historical workbook, scenario and printed report-table layouts,
  including joint reports, illustrative scenarios, constant-rate conditioning,
  backcast variants and multipage annual profiles. Exact older vintages can be
  supplied by later official historical sources with explicit provenance.
* Retains publication-date precision, original missing/censored tokens,
  probability bounds, raw source labels, exact cells/pages and revision receipts.
* Adds bounded read-only archive search POST caching, caller-owned deadlines,
  XLSX preflight and safe ZIP streaming. Legacy GET cache identities and the
  generic workbook reader's default bounds remain unchanged.
* Explicit raw historical report PDF extraction requires Poppler's `pdftotext` on PATH.

# readboe 0.1.6

* Standardises package author, maintainer, and copyright-holder metadata as
  Chartwell Macro.
* Adds a deterministic end-to-end example that discovers monthly SONIA,
  replays a verified official IADB response through `read_boe_series()`, and
  produces a labelled `ggplot2` chart of its full January 1997–December 2025
  catalogue history in the README and Getting Started article.
* Adds an upfront coverage map with audited Bankstats subject, frequency,
  series, workbook, rate, yield-curve, release-family, and historical-range
  boundaries.
* Refreshes the user documentation with a source and workflow map, explicit
  output contracts, authentication/network/cache guidance, limitations, and
  troubleshooting information.
* Adds deterministic offline examples and task-oriented vignettes for the
  package's supported retrieval and provenance workflows.
* Adds a task-oriented pkgdown configuration and documentation/quality checks,
  with contributor, citation, and licensing guidance where applicable.
* Continues CRAN-readiness work through portable metadata, generated
  references, clean source-build hygiene, and offline-safe checks; live source
  availability and any release-asset gaps remain documented limitations.
* Makes Bankstats English-month and HTTP `Retry-After` date parsing independent
  of `LC_TIME`, preserving the existing results in English locales while
  making the same official inputs portable to other locales.

# readboe 0.1.5

* Replaces rolling-workbook date windows with canonical IADB first/last
  observation bounds and full XML history for Bankstats series.
* Expands the authenticated browse snapshot to every exact, canonically
  retrievable identity linked by the official current Bankstats tables page.
  Nine invalid/replaced upstream links are affirmatively recorded and excluded;
  workbook chartability is a secondary capability flag.
* Preserves canonical titles, frequencies, categories, units, seasonal
  adjustment, provisional/confidential values, update/revision fields, raw
  missing markers, table membership, and exact source receipts.
* Adds a reproducible all-series and table-by-table history audit with bounded,
  checkpointed official metadata requests.

# readboe 0.1.4

* Constrains time-across period inference to the official header block above
  the series-prefix row, preventing numeric observations in preceding country
  rows from being misread as years.
* Adds a regression fixture and catalogue-wide date plausibility contract for
  every workbook-backed browse identity.

# readboe 0.1.3

* Prefers the first official workbook occurrence when one series appears on
  several sheets, preventing secondary header fragments from becoming the
  browse title.
* Normalizes simple workbook heading syntax such as `By instrument`,
  `By sector`, and redundant `Bonds/Shares/Total paper` suffixes without
  changing source identity or observation data.

# readboe 0.1.2

* Narrows the current Bankstats browse snapshot to 5,009 identities proved
  chartable in the 64 pinned bulk workbooks; 252 page-linked identities without
  an exact current workbook occurrence are excluded.
* Adds workbook-derived human-readable series labels, frequency, date range,
  sheet, orientation, and table metadata to the authenticated offline catalogue.
* Accepts annotated workbook code cells while retaining exact series-prefix,
  table, period, and cell identity checks.

# readboe 0.1.1

* Adds `read_boe_bankstats_series()` for exact current-series extraction from
  both time-down and time-across Bankstats bulk workbook layouts.
* Retains raw missing/suppression markers, official period labels, table/member/
  sheet/cell identity, bulk-file hashes, and the source-page receipt.
* Adds deterministic schema/bounds fixtures and live canaries for monthly and
  quarterly current Bankstats series.

# readboe 0.1.0

* First release.
* Adds bounded, provenance-bearing IADB XML, CSV, and VUD retrieval.
* Adds authenticated offline browsing for the explicitly scoped current Bankstats catalogue (5,261 unique series at the initial snapshot), with exact facet filters.
* Deliberately excludes non-Bankstats IADB and discontinued identities from the staged browse snapshot; these may be added in a later release.
* Adds Bankstats table discovery and bounded bulk-workbook parsing.
* Adds official Bank Rate and all current and archived yield-curve families.
* Adds an audited registry for stable machine-readable statistical releases.
