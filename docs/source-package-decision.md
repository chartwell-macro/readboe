# Bank of England source and R-package decision

Audit date: 2026-08-03 (Australia/Sydney). Only official Bank of England hosts are treated as data authorities. CRAN is used only to inventory R packages.

## Decision

Create `readboe`. No existing maintained R package provides the required combined coverage and safety contract.

### Bankstats browse scope accepted on 2026-08-03

The official current Bankstats tables page contains 5,261 unique identities from 5,465 links across 60 tables. Canonical IADB XML validates 5,252; eight page links are rejected by the Bank as invalid series codes and one link resolves to a replacement notice with no observation bounds. Those nine are affirmatively recorded in `docs/audits` and excluded from browse rather than exposed as dead results. The rolling current workbooks contain an extractable occurrence for 5,008 retained identities; the other 244 remain in browse and use canonical IADB history, while workbook mode fails closed. Non-Bankstats IADB and discontinued identities remain excluded under the user's accepted scope rather than inferred.

The official IADB XML surface is canonical because it alone carries the published series metadata, footnotes, confidentiality markers, provisional values, last-update/revision fields, and VUD new/revised observations. CSV is retained as a compatibility surface and reconciled only on non-missing values. Bankstats bulk workbooks, Bank Rate history, fitted yield-curve files, and identity-stable release assets are separate official surfaces and cannot be truthfully represented by a CSV-only IADB wrapper.

## Package registry audit

The CRAN source registry was searched by exact and related terms. Three packages expose Bank of England retrieval code:

| Package | Audited version/source SHA-256 | What it covers | Why it does not satisfy the contract |
|---|---|---|---|
| `boe` | 0.3.0; `d9dc71400a9800a8e839c1415137ba010ec132856d7feaafa559e12ae62b1509` | Convenience functions over IADB CSV; a hand-curated 52-series catalogue; partial fitted-yield helpers | Its IADB parser explicitly drops NA values; catalogue coverage is 52 series, not the official catalogue; it does not preserve XML metadata/revision/VUD fields; cache identity is a non-cryptographic checksum; no caller absolute deadline, aggregate cache bound, typed schema/identity errors, Bankstats catalogue/bulk contract, release registry, or exact receipts. Its yield parser removes NA rates and collapses duplicate date/maturity rows. |
| `bbk` | 0.12.0; `4b182097d454520c226109e0f4a83800bd67c30065dab4641ed64adfdeaa55c0` | Generic central-bank client with a direct IADB XML function | Its BoE parser retains only ID, description, frequency, selected categories, date, and numeric value. It omits raw/suppressed/provisional values, confidentiality, footnotes, series comments/definitions, first/last observation, update/revision fields and VUD behavior. It has no official catalogue discovery, Bankstats, Bank Rate workbook, fitted-yield files, release assets, or package-specific offline/provenance/schema contracts. |
| `pdfetch` | 0.3.3; `ca20491b039893be6e4ffb6c5612261e695df525252419abe6e4c7f7f9dfd291` | Legacy `pdfetch_BOE()` IADB CSV helper | It constructs a plain-HTTP legacy URL, parses a narrow CSV result, and has none of the required discovery, metadata/revision, allowlist/deadline/bounds/cache/provenance, Bankstats, rates, yields, or release coverage. |

These packages remain useful for narrower use cases; duplicating their convenience APIs is not the goal. `readboe` exists to own the full official-source and replay contract needed by Chartwell.

## Official surface audit

### IADB

The official help page documents a maximum of 300 series for XML/CSV requests, canonical XML metadata, provisional and footnote controls, suppressed/confidential `..` observations (`OBS_CONF="Y"`), CSV layouts, and VUD modes A (all plus updated markers) and B (updated only). The update baseline is 3 June 2007. XML—not CSV—supports historical publication/revision information.

For the accepted Bankstats scope, discovery comes exclusively from exact `EC=` identities on the official current tables page. Canonical XML metadata is then fetched in 25-series requests below the practical GET-length ceiling. Each request is exact-set checked, byte bounded, URL identified, and SHA-256 receipt-hashed. The manifest records the page count/hash, metadata request hashes, and exact catalogue snapshot hash.

Full-text code-prefix search was audited and rejected as a catalogue authority: even a broad seed/frequency-window family inventory reproduced only 18,960 of 30,943 non-seasonally-adjusted IDs. The legacy form also treats `_` as a wildcard and returns a reproducible HTTP 500 for at least the `RPM*` monthly query. These behaviors are recorded as upstream quirks, not trusted as identity partitions.

### Bankstats

The current tables page links one `bankstats-latest-tables.zip`. The audited ZIP SHA-256 is `ec37bc98fbab6e55916f247732c01cbcdc47e6f978f541a6bbba1387227f52ca`; it contains one directory plus exactly 64 `.xls` workbooks spanning table families A through G. The page exposes exact `EC=` identities for 60 of those tables. Four workbook tables (`C3.1`, `C3.4`, `C4.2`, and `G1.4`) expose no exact page-linked identity and are therefore available only through raw bounded bulk parsing, not invented browse identities. The official page and statistics sitemap do not expose a dated historical bulk-ZIP archive. Full series history comes from canonical IADB XML; rolling workbooks remain the current publication view.

The discontinued-series workbook SHA-256 is `1f3b8b22f987d2b8d451f875c74a8fa54e4da32f92719558691db45d50f619e1`; its published last-updated cell is 2021-07-19 and it contains 14,258 exact identifiers plus latest-period/replacement/note fields.

### Bank Rate and IADB rates

The official `baserate.xls` SHA-256 is `fc910d349d661e17e26238f250f6573bdf1840d30a764badda1f40bf057f24e0`. Its historical sheet contains 852 published regime changes from October 1694 through 18 December 2025. The first 17 are month precision only and remain `date = NA` with exact `period`/`date_precision`; no day is invented. IADB exact identifiers cover daily/monthly Bank Rate and daily/monthly/annual SONIA.

### Fitted yield curves

The official page publishes 11 fixed file identities: the latest daily bundle and daily/monthly archives for nominal, real, implied inflation, OIS, and discontinued BLC curves. The Bank says nominal daily coverage starts in 1979, real/inflation in 1985, OIS in 2009, and BLC ends in 2021; archive revisions are published monthly. Workbooks contain spot/forward and standard/short-end sheets. `readboe` retains NA cells and official duplicate dates using workbook/member/row/column identity.

Audited archive SHA-256 values:

| Family | Daily | Monthly |
|---|---|---|
| Nominal | `11a7592de94cfbe9a3f7fed9d51417188b7e8e6a4110eb2f56bd1af592965cde` | `6e17178368bafb1ab849168c361594e62f994d221764aaeb9709967e4676f002` |
| Real | `d8739eba2ca406d7fb0a92ef1f98bc1230a4a9ae025d209260e203350dda1f33` | `97f9b4a10be2a48acf0720fa554ae0ab44cadffbd0f37ec74329492e2ce9fcd0` |
| Inflation | `2833168a866eadf7ff9d2c2918025d881e3e8a751abb5df694df778e57e3c5f6` | `13a3ef3579053bfbd275ca7ad31bd109163a3263646e0e64f4dce39061789ebc` |
| OIS | `4430437036485dea5ff2592c39d8ad55db5ca1115c3703c93348961f2fa16fae` | `2705ca8e974e5e985d1dfd1ec222599e1f9dc3580361506fa709b76aff5d8bb8` |
| BLC | `56b882dbc8183a04842546df80353176228a173c7a35e7be3fdb3943df3e5704` | `c98864e08ae1f15ab3c9d1070b3930238467bc13515052881784cc3c00408a09` |

Latest daily bundle SHA-256: `fbe049b58c3606ea19d8c4e0174f46a0c3bd6be9a365a1c0c7c57b2f1418b6e9`. Exact row/date/NA/duplicate/object-size evidence is in `docs/live-yield-certification.csv`.

### Other statistical and research assets

The package snapshots only official HTTPS assets with stable file identity and a machine-readable extension. Fourteen audited families are classified: requested data, GDP real-time, research datasets, payments, gold custody, banknotes, international reserves, MLAR, capital issuance, credit unions, insurance annual, insurance quarterly, external business, and external claims. XLS/XLSX files have a lossless raw-sheet parser; XLSB, CSV, ZIP, and heterogeneous research bundles are downloadable as exact verified raw bytes unless a more specific parser is declared. This is intentional—no inferred schema is imposed on heterogeneous research files.

The former `/payment-and-settlement/payment-and-settlement-statistics` page is an official 404. Discovery uses the current `/payments/payment-and-settlement-statistics` route, which exposes the historic CHAPS workbook. Route migration is recorded rather than silently followed as an equivalent identity.

## Safety and semantics decision

`readboe` preserves official IDs, labels, frequency, categories/dimensions, raw missing/suppression markers, provisional values, revision/update fields, and workbook source coordinates. It performs no aggregation, interpolation, seasonal adjustment, NA dropping, or duplicate collapse. Transport is official-host HTTPS only, exact-query keyed, success-cache only, hash verified, TTL/count/aggregate-byte bounded, retry-limited to transient failures and `Retry-After`, and constrained by one caller-owned absolute deadline plus per-attempt timeouts. Conditions are typed for configuration/auth, timeout, transport, rate limit, bounds, parse/schema drift, identity mismatch, cache miss, and cache corruption.

## Primary evidence

* IADB help: <https://www.bankofengland.co.uk/boeapps/database/Help.asp?Highlight=Hierarchy&Travel=>
* IADB advanced search: <https://www.bankofengland.co.uk/boeapps/database/advsearch.asp?Travel=NIx>
* Bankstats tables: <https://www.bankofengland.co.uk/statistics/tables>
* Yield curves: <https://www.bankofengland.co.uk/statistics/yield-curves>
* Yield concepts/coverage: <https://www.bankofengland.co.uk/statistics/yield-curves/terminology-and-concepts>
* Bank Rate: <https://www.bankofengland.co.uk/monetary-policy/interest-rates-and-bank-rate>
* Statistics sitemap: <https://www.bankofengland.co.uk/sitemap/statistics>
* Legal terms: <https://www.bankofengland.co.uk/legal>
* CRAN `boe`: <https://cran.r-project.org/package=boe>
* CRAN `bbk`: <https://cran.r-project.org/package=bbk>
* CRAN `pdfetch`: <https://cran.r-project.org/package=pdfetch>
