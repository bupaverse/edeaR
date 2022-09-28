# edeaR 0.9.1 (dev)

## Features

* `filter_infrequent_flows()`'s argument `min_n` now defaults to `2`.
* `filter_activity_presence()` emits warning when specified activities cannot be found in the log. When no valid activities are specified, an empty log is returned. 

## Bug Fixes

* Fixed bug when plotting `processing_time()`, `throughput_time()`, or `idle_time()` with argument
`units = "auto"` (default) which caused units to be ambiguously displayed as "auto" in the graph. (#36).
* Fixed bug in `plot()` which failed when plotting `processing_time()`, `throughput_time()`, and `idle_time()`
with argument `level = "case"` (e.g. `patients %>% processing_time(level = "case") %>% plot()`) (#37).
* Summary statistics of metrics at `level = "log"` with time units (e.g. `processing_time()`,
`throughput_time()`, or `idle_time()`) now retain the units in the output.
* `filter_processing_time()` no longer shows the `Warning: between() called on numeric vector with S3 class` 
when an `interval` is supplied.
* Fixed bug in `filter_infrequent_flows()` which failed when applied to an `activitylog`.
* Fixed bug in `filter_activity_presence()` which failed when applied to an `grouped_log`.
* Fixed bug in `filter_trim()` which failed when applied to an `grouped_log`.


## Other

* Added a `NEWS.md` file to track changes to the package.