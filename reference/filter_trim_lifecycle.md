# Filter Trim Life Cycle

Trim activity instances from the first event of a set of start life
cycle labels to the last event of a set of end life cycle labels.

One can trim activity instances by removing one or more events at the
start and/or end of the activity instances. Trimming is performed until
all activity instances have a start and/or end point belonging to a set
of allowed life cycle labels. This filter requires a set of allowed
start life cycle labels and/or a set of allowed life cycle labels. If
one of them is not provided it will not trim the activity instances at
this edge.The selection can be reversed, which means that only the
trimmed events at the start and end of activity instances are retained.
As such, this argument allows to cut intermediate parts out of activity
instances.

## Usage

``` r
filter_trim_lifecycle(
  log,
  start_lifecycles = NULL,
  end_lifecycles = NULL,
  reverse = FALSE
)

# S3 method for class 'eventlog'
filter_trim_lifecycle(
  log,
  start_lifecycles = NULL,
  end_lifecycles = NULL,
  reverse = FALSE
)

# S3 method for class 'grouped_eventlog'
filter_trim_lifecycle(
  log,
  start_lifecycles = NULL,
  end_lifecycles = NULL,
  reverse = FALSE
)

ifilter_trim_lifecycle(log)
```

## Arguments

- log:

  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html):
  Object of class
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html)
  or derivatives
  ([`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html)).

- start_lifecycles, end_lifecycles:

  [`character`](https://rdrr.io/r/base/character.html) vector (default
  [`NULL`](https://rdrr.io/r/base/NULL.html)): A vector of life cycle
  identifiers, or [`NULL`](https://rdrr.io/r/base/NULL.html).

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

## Methods (by class)

- `filter_trim_lifecycle(eventlog)`: Filters activity instances for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `filter_trim_lifecycle(grouped_eventlog)`: Filters activity instances
  for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

## Functions

- `ifilter_trim_lifecycle()`: Filter interactively

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`lifecycle_id`](https://bupaverse.github.io/bupaR/reference/lifecycle_id.html)

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
[`filter_time_period()`](https://bupaverse.github.io/edeaR/reference/filter_time_period.md),
[`filter_trace()`](https://bupaverse.github.io/edeaR/reference/filter_trace.md),
[`filter_trace_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_trace_frequency.md),
[`filter_trace_length()`](https://bupaverse.github.io/edeaR/reference/filter_trace_length.md),
[`filter_trim()`](https://bupaverse.github.io/edeaR/reference/filter_trim.md)
