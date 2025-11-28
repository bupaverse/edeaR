# Filter Time Period

Function to filter the log using a time period.

## Usage

``` r
filter_time_period(
  log,
  interval = NULL,
  filter_method = c("contained", "intersecting", "start", "complete", "trim"),
  force_trim = FALSE,
  reverse = FALSE
)

# S3 method for class 'eventlog'
filter_time_period(
  log,
  interval = NULL,
  filter_method = c("contained", "intersecting", "start", "complete", "trim"),
  force_trim = FALSE,
  reverse = FALSE
)

# S3 method for class 'grouped_eventlog'
filter_time_period(
  log,
  interval = NULL,
  filter_method = c("contained", "intersecting", "start", "complete", "trim"),
  force_trim = FALSE,
  reverse = FALSE
)

# S3 method for class 'activitylog'
filter_time_period(
  log,
  interval = NULL,
  filter_method = c("contained", "intersecting", "start", "complete", "trim"),
  force_trim = FALSE,
  reverse = FALSE
)

# S3 method for class 'grouped_activitylog'
filter_time_period(
  log,
  interval = NULL,
  filter_method = c("contained", "intersecting", "start", "complete", "trim"),
  force_trim = FALSE,
  reverse = FALSE
)

ifilter_time_period(log)
```

## Arguments

- log:

  [`log`](https://bupaverse.github.io/bupaR/reference/log.html): Object
  of class [`log`](https://bupaverse.github.io/bupaR/reference/log.html)
  or derivatives
  ([`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html),
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html),
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html),
  etc.).

- interval:

  [`Date`](https://rdrr.io/r/base/Dates.html) or
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html) vector: A
  time interval (vector of length 2 of type
  [`Date`](https://rdrr.io/r/base/Dates.html) or
  [`POSIXct`](https://rdrr.io/r/base/DateTimeClasses.html)). Half-open
  intervals can be created with [`NA`](https://rdrr.io/r/base/NA.html).

- filter_method:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"contained"`): Filtering method: `"contained"` (default),
  `"intersecting"`, `"start"`, `"complete"`, or `"trim"`. For more
  information, see 'Details' below.

- force_trim:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `FALSE`): If
  `TRUE` in combination with `filter_method` `"trim"`, activity
  instances on the edges of the interval are cut at the exact edge of
  the `interval`.

- reverse:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `FALSE`):
  Indicating whether the selection should be reversed.

## Value

When given an object of type
[`log`](https://bupaverse.github.io/bupaR/reference/log.html), it will
return a filtered
[`log`](https://bupaverse.github.io/bupaR/reference/log.html). When
given an object of type
[`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html),
the filter will be applied in a stratified way (i.e. each separately for
each group). The returned log will be grouped on the same variables as
the original log.

## Details

Event data can be filtered by supplying a time window to the method
`filter_time_period`. There are 5 different values for `filter_method`:

- `"contained"`: Keeps all the events related to cases contained in the
  time period.

- `"intersecting"`: Keeps all the events related to cases in which at
  least one event started and/or ended in the time period.

- `"start"`: Keeps all the events related to cases started in the time
  period.

- `"complete"`: Keeps all the events related to cases complete in the
  time period.

- `"trim"`: Keeps all the events which started and ended in the time
  frame.

## Methods (by class)

- `filter_time_period(eventlog)`: Filters activity instances for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `filter_time_period(grouped_eventlog)`: Filters activity instances for
  a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `filter_time_period(activitylog)`: Filters activity instances for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `filter_time_period(grouped_activitylog)`: Filters activity instances
  for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## Functions

- `ifilter_time_period()`: Filter interactively

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

Other filters:
[`filter_activity()`](https://bupaverse.github.io/edeaR/reference/filter_activity.md),
[`filter_activity_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_activity_frequency.md),
[`filter_activity_instance()`](https://bupaverse.github.io/edeaR/reference/filter_activity_instance.md),
[`filter_activity_presence()`](https://bupaverse.github.io/edeaR/reference/filter_activity_presence.md),
[`filter_case()`](https://bupaverse.github.io/edeaR/reference/filter_case.md),
[`filter_case_condition()`](https://bupaverse.github.io/edeaR/reference/filter_case_condition.md),
[`filter_endpoints()`](https://bupaverse.github.io/edeaR/reference/filter_endpoints.md),
[`filter_endpoints_condition()`](https://bupaverse.github.io/edeaR/reference/filter_endpoints_condition.md),
[`filter_flow_time()`](https://bupaverse.github.io/edeaR/reference/filter_flow_time.md),
[`filter_idle_time()`](https://bupaverse.github.io/edeaR/reference/filter_idle_time.md),
[`filter_infrequent_flows()`](https://bupaverse.github.io/edeaR/reference/filter_infrequent_flows.md),
[`filter_lifecycle()`](https://bupaverse.github.io/edeaR/reference/filter_lifecycle.md),
[`filter_lifecycle_presence()`](https://bupaverse.github.io/edeaR/reference/filter_lifecycle_presence.md),
[`filter_precedence()`](https://bupaverse.github.io/edeaR/reference/filter_precedence.md),
[`filter_precedence_condition()`](https://bupaverse.github.io/edeaR/reference/filter_precedence_condition.md),
[`filter_precedence_resource()`](https://bupaverse.github.io/edeaR/reference/filter_precedence_resource.md),
[`filter_processing_time()`](https://bupaverse.github.io/edeaR/reference/filter_processing_time.md),
[`filter_resource()`](https://bupaverse.github.io/edeaR/reference/filter_resource.md),
[`filter_resource_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_resource_frequency.md),
[`filter_throughput_time()`](https://bupaverse.github.io/edeaR/reference/filter_throughput_time.md),
[`filter_trace()`](https://bupaverse.github.io/edeaR/reference/filter_trace.md),
[`filter_trace_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_trace_frequency.md),
[`filter_trace_length()`](https://bupaverse.github.io/edeaR/reference/filter_trace_length.md),
[`filter_trim()`](https://bupaverse.github.io/edeaR/reference/filter_trim.md),
[`filter_trim_lifecycle()`](https://bupaverse.github.io/edeaR/reference/filter_trim_lifecycle.md)
