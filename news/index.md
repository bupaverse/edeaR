# Changelog

## edeaR 1.0.0

- Updated pre-defined visualizations
- Interactive filtering has improved and now returns the used filter
  call for reproducibility
- Deprecated functionalities are defintely removed
  - Use of append in metrics. Use `augment` instead
  - Use of eventlog argument in all functions. Use argument `log`
    instead

## edeaR 0.9.4

CRAN release: 2023-04-27

### Bugfixes

- Rework functions (`number_of_selfloops`, `number_of_repetitions`,
  `size_of_selfloops`, `size_of_repetitions`) have been fixed to be
  compatible with changes in bupaR 0.5.3.

## edeaR 0.9.2

CRAN release: 2023-01-15

### Breaking Changes

- [`filter_activity_presence()`](https://bupaverse.github.io/edeaR/reference/filter_activity_presence.md)
  no longer ignores unknown activities. A warning is thrown when the
  specified activities were not found in the log.
- `units = "auto"` is no longer an available option for
  [`filter_throughput_time()`](https://bupaverse.github.io/edeaR/reference/filter_throughput_time.md)
  and
  [`filter_processing_time()`](https://bupaverse.github.io/edeaR/reference/filter_processing_time.md),
  as it does not make sense to have units automatically defined when
  using a duration interval.
- Column `"relative_trace_frequency"` of
  [`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md)
  with argument `level = "trace"` is now renamed to
  `"relative_frequency"` to be more consistent with other methods/levels
  reporting on relative frequency.

### Features

- [`filter_idle_time()`](https://bupaverse.github.io/edeaR/reference/filter_idle_time.md)
  is a new filter, in analogy with
  [`filter_processing_time()`](https://bupaverse.github.io/edeaR/reference/filter_processing_time.md)
  and
  [`filter_throughput_time()`](https://bupaverse.github.io/edeaR/reference/filter_throughput_time.md).
- [`filter_flow_time()`](https://bupaverse.github.io/edeaR/reference/filter_flow_time.md):
  A new filter function that allows to select cases where the activity
  `from` is followed by the activity `to` within a certain time
  `interval`.
- [`filter_activity_presence()`](https://bupaverse.github.io/edeaR/reference/filter_activity_presence.md)
  emits a warning when one or more of the specified activities cannot be
  found in the log, but will still take all specified activities into
  account when filtering.
- [`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md)
  performance has been significantly improved.

### Bugfixes

- [`filter_time_period()`](https://bupaverse.github.io/edeaR/reference/filter_time_period.md)
  now also works correctly with open-ended intervals, using `NA`.
- [`filter_endpoints_condition()`](https://bupaverse.github.io/edeaR/reference/filter_endpoints_condition.md)
  now also works correctly on grouped event logs. Note that the
  deprecated
  [`filter_endpoints_conditions()`](https://bupaverse.github.io/edeaR/reference/filter_endpoints_condition.md)
  only works with ordinary logs.
- [`resource_involvement()`](https://bupaverse.github.io/edeaR/reference/resource_involvement.md)
  at level resource-activity now correctly outputs list of
  resource-activity, instead of just resources.

## edeaR 0.9.1

CRAN release: 2022-10-03

### Features

- [`filter_infrequent_flows()`](https://bupaverse.github.io/edeaR/reference/filter_infrequent_flows.md)’s
  argument `min_n` now defaults to `2`.
- [`filter_activity_presence()`](https://bupaverse.github.io/edeaR/reference/filter_activity_presence.md)
  emits warning when specified activities cannot be found in the log.
  When no valid activities are specified, an empty log is returned.

### Bug Fixes

- Fixed bug when plotting
  [`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),
  [`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
  or
  [`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md)
  with argument `units = "auto"` (default) which caused units to be
  ambiguously displayed as “auto” in the graph.
  ([\#36](https://github.com/bupaverse/edeaR/issues/36)).
- Fixed bug in
  [`plot()`](https://bupaverse.github.io/edeaR/reference/plot.md) which
  failed when plotting
  [`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),
  [`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
  and
  [`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md)
  with argument `level = "case"`
  (e.g. `patients %>% processing_time(level = "case") %>% plot()`)
  ([\#37](https://github.com/bupaverse/edeaR/issues/37)).
- Summary statistics of metrics at `level = "log"` with time units
  (e.g. [`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),
  [`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
  or
  [`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md))
  now retain the units in the output.
- [`filter_processing_time()`](https://bupaverse.github.io/edeaR/reference/filter_processing_time.md)
  no longer shows the
  `Warning: between() called on numeric vector with S3 class` when an
  `interval` is supplied.
- Fixed bug in
  [`filter_infrequent_flows()`](https://bupaverse.github.io/edeaR/reference/filter_infrequent_flows.md)
  which failed when applied to an `activitylog`.
- Fixed bug in
  [`filter_activity_presence()`](https://bupaverse.github.io/edeaR/reference/filter_activity_presence.md)
  which failed when applied to an `grouped_log`.
- Fixed bug in
  [`filter_trim()`](https://bupaverse.github.io/edeaR/reference/filter_trim.md)
  which failed when applied to an `grouped_log`.
- Functions `resource_frequency`, `resource_specialization` and
  `resource_involvement` have been corrected, as they showed erroneous
  results as of bupaR 0.5.0.

### Other

- Added a `NEWS.md` file to track changes to the package.
