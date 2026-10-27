# readboe 0.2.0

The release adds saved Inflation Report and Monetary Policy Report forecasts,
including historical editions, scenarios, conditioning assumptions and published
distributions. Ordinary reads use the authenticated numeric snapshot. Explicit
updates maintain immutable generations, retain last-good data on failure and
check official publication dates and historical revisions.

The reviewed snapshot contains 135 published editions from 1993 through July
2026: 115 numeric outcomes, 20 documented absences and 149,526 observations
including history. Exact periods, source labels, missing/censored values and
source receipts remain available. New releases require an explicit update.

Matched five-pair full-corpus measurements against the earlier saved-reader
implementation found a cold median of 0.4562 seconds versus 0.5004 seconds;
warm median reads took 1.2897 ms versus 1.2876 ms. Median peak process memory
was 441.25 MiB versus 430.92 MiB. Returned values, dates, attributes and
provenance were identical, and normal reads performed no network requests or
persistent writes. The small warm-read and process-memory differences are
accepted release tradeoffs; exact performance parity is not claimed.

Package software is MIT-licensed. The publisher has confirmed permission to
redistribute this bundled snapshot; applicable Bank and third-party notices
remain in inst/COPYRIGHTS and inst/NOTICE. This does not grant a general licence
for other datasets or uses.

Support: [chartwellmacro@gmail.com](mailto:chartwellmacro@gmail.com).
