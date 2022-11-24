# edeaR 0.9.2.9000 (dev)

## Breaking Changes

* `filter_activity_presence()` no longer ignores unknown activities. A warning is thrown when the specified activity/ies were not found in the log.
* `units = "auto"` is no longer an available option for `filter_throughput_time` and `filter_processing_time`, as it does not make sense to have units automatically defined when using a duration interval. 

## Features

*  `filter_idle_time` is a new filter, in analogy with `filter_processing_time` and `filter_throughput_time`. 

## Bugfixes

*  `filter_time_period` now also works correctly with open-ended intervals, using `NA`. 


# edeaR 0.9.1

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
* Functions `resource_frequency`, `resource_specialization` and `resource_involvement` have been corrected, as they showed erroneous results as of bupaR 0.5.0. 


## Other

* Added a `NEWS.md` file to track changes to the package.
