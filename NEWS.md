# edeaR 0.9.1 (dev)

## Features

## Bug Fixes

* Fixed bug when plotting `processing_time()`, `throughput_time()`, or `idle_time()` with argument
`units = "auto"` (default) which caused units to be ambiguously displayed as "auto" in the graph. (#36).
* Fixed bug in `plot()` which failed when plotting `processing_time()`, `throughput_time()`, and `idle_time()`
with argument `level = "case"` (e.g. `patients %>% processing_time(level = "case") %>% plot()`) (#37).
* Summary statistics of metrics at `level = "log"` with time units (e.g. `processing_time()`,
`throughput_time()`, or `idle_time()`) now retain the units in the output.
* `filter_processing_time()` no longer shows the `Warning: between() called on numeric vector with S3 class` 
when an `interval` is supplied.

## Other

* Added a `NEWS.md` file to track changes to the package.
