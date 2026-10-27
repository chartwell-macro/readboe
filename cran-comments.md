# Release and CRAN-readiness notes

This is the public GitHub release of readboe 0.2.0. It is not a CRAN submission.

The release preserves the certified 0.1.6 Bankstats, IADB, Bank Rate, yield-curve
and SONIA documentation interfaces and adds saved MPR/Inflation Report forecasts,
explicit raw reconstruction, updates and calendar-aware maintenance.

The complete offline public-staging suite and final built-package checks are
recorded in the release receipt. Official-source and live checks are opt-in;
they are not misrepresented as part of deterministic CI. The existing missing
generic release-asset snapshot remains an explicitly documented limitation.

Exact-source independent review approved correctness and the measured practical
performance tradeoff: cold median reads improved by 8.83%; warm median changed
by +2.05 microseconds (+0.159%); process peak RSS median increased by 10.33 MiB
(2.397%). Returned values, dates, provenance, snapshot sizes and ordinary-read
network/write counts were preserved. Exact performance parity is not claimed.

The publisher has confirmed permission to redistribute the bundled forecast
snapshot. MIT covers package software; Bank and third-party source rights,
attribution and notices remain separate in inst/COPYRIGHTS and inst/NOTICE.

Before any future CRAN submission, recheck current CRAN policy, source/data
notices, maintainer correspondence, remote URLs and the then-current R/OS matrix.
