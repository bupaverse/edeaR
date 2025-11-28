# Filter Resource Frequency

Filters the log based on frequency of resources

## Usage

``` r
filter_resource_frequency(
  log,
  interval = NULL,
  percentage = NULL,
  reverse = FALSE
)

# S3 method for class 'log'
filter_resource_frequency(
  log,
  interval = NULL,
  percentage = NULL,
  reverse = FALSE
)

# S3 method for class 'grouped_log'
filter_resource_frequency(
  log,
  interval = NULL,
  percentage = NULL,
  reverse = FALSE
)

ifilter_resource_frequency(log)
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

- percentage, interval:

  The target coverage of activity instances. Provide either `percentage`
  or `interval`.  
  `percentage` ([`numeric`](https://rdrr.io/r/base/numeric.html)): A
  percentile of p will return the most common resource types of the log,
  which account for at least p% of the activity instances.  
  `interval` ([`numeric`](https://rdrr.io/r/base/numeric.html) vector of
  length 2): A resource frequency interval. Half open interval can be
  created using [`NA`](https://rdrr.io/r/base/NA.html).  
  For more information, see 'Details' below.

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

Filtering the log based on resource frequency can be done in two ways:
using an `interval` of allowed frequencies, or specify a coverage
`percentage`:

- `percentage`: When filtering using a percentage p%, the filter will
  return p% of the activity instances, starting from the resource labels
  with the highest frequency. The filter will retain additional resource
  labels as long as the number of activity instances does not exceed the
  percentage threshold.

- `interval`: When filtering using an interval, resource labels will be
  retained when their absolute frequency fall in this interval. The
  interval is specified using a numeric vector of length 2. Half open
  intervals can be created by using
  [`NA`](https://rdrr.io/r/base/NA.html), e.g., `c(10, NA)` will select
  resource labels which occur 10 times or more.

## Methods (by class)

- `filter_resource_frequency(log)`: Filters resources for a
  [`log`](https://bupaverse.github.io/bupaR/reference/log.html).

- `filter_resource_frequency(grouped_log)`: Filters resources for a
  [`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html).

## Functions

- `ifilter_resource_frequency()`: Filter interactively

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
[`filter_throughput_time()`](https://bupaverse.github.io/edeaR/reference/filter_throughput_time.md),
[`filter_time_period()`](https://bupaverse.github.io/edeaR/reference/filter_time_period.md),
[`filter_trace()`](https://bupaverse.github.io/edeaR/reference/filter_trace.md),
[`filter_trace_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_trace_frequency.md),
[`filter_trace_length()`](https://bupaverse.github.io/edeaR/reference/filter_trace_length.md),
[`filter_trim()`](https://bupaverse.github.io/edeaR/reference/filter_trim.md),
[`filter_trim_lifecycle()`](https://bupaverse.github.io/edeaR/reference/filter_trim_lifecycle.md)
