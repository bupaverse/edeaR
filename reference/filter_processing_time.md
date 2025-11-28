# Filter Processing Time

Filters cases based on their
[`processing_time`](https://bupaverse.github.io/edeaR/reference/processing_time.md).

This filter can be used by using an `interval` or by using a
`percentage`. The percentage will always start with the shortest cases
first and stop including cases when the specified percentile is reached.
On the other hand, an absolute interval can be defined instead to filter
cases which have a processing time in this interval. The time units in
which this interval is defined can be supplied with the `units`
argument.

## Usage

``` r
filter_processing_time(
  log,
  interval = NULL,
  percentage = NULL,
  reverse = FALSE,
  units = c("secs", "mins", "hours", "days", "weeks")
)

# S3 method for class 'log'
filter_processing_time(
  log,
  interval = NULL,
  percentage = NULL,
  reverse = FALSE,
  units = c("secs", "mins", "hours", "days", "weeks")
)

# S3 method for class 'grouped_log'
filter_processing_time(
  log,
  interval = NULL,
  percentage = NULL,
  reverse = FALSE,
  units = c("secs", "mins", "hours", "days", "weeks")
)

ifilter_processing_time(log)
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

- interval, percentage:

  Provide either `interval` or `percentage`.  
  `interval` ([`numeric`](https://rdrr.io/r/base/numeric.html) vector of
  length 2): A duration interval. Half open interval can be created
  using [`NA`](https://rdrr.io/r/base/NA.html).  
  `percentage` ([`numeric`](https://rdrr.io/r/base/numeric.html)): A
  percentage to be used for relative filtering.

- reverse:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `FALSE`):
  Indicating whether the selection should be reversed.

- units:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"secs"`): The time unit in which the processing times should be
  reported. Should be one of the following values: `"secs"` (default),
  `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the `units`
  argument of [`difftime()`](https://rdrr.io/r/base/difftime.html).

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

## Methods (by class)

- `filter_processing_time(log)`: Filters cases for a
  [`log`](https://bupaverse.github.io/bupaR/reference/log.html).

- `filter_processing_time(grouped_log)`: Filters cases for a
  [`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html).

## Functions

- `ifilter_processing_time()`: Filter interactively

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),[`difftime()`](https://rdrr.io/r/base/difftime.html)

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
[`filter_resource()`](https://bupaverse.github.io/edeaR/reference/filter_resource.md),
[`filter_resource_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_resource_frequency.md),
[`filter_throughput_time()`](https://bupaverse.github.io/edeaR/reference/filter_throughput_time.md),
[`filter_time_period()`](https://bupaverse.github.io/edeaR/reference/filter_time_period.md),
[`filter_trace()`](https://bupaverse.github.io/edeaR/reference/filter_trace.md),
[`filter_trace_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_trace_frequency.md),
[`filter_trace_length()`](https://bupaverse.github.io/edeaR/reference/filter_trace_length.md),
[`filter_trim()`](https://bupaverse.github.io/edeaR/reference/filter_trim.md),
[`filter_trim_lifecycle()`](https://bupaverse.github.io/edeaR/reference/filter_trim_lifecycle.md)
