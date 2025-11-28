# Trim Cases

Trim cases from the first event of a set of start activities to the last
event of a set of end activities.

One can trim cases by removing one or more activity instances at the
start and/or end of a case. Trimming is performed until all cases have a
start and/or end point belonging to a set of allowed activity labels.
This filter requires a set of allowed start activities and/or a set of
allowed end activities. If one of them is not provided it will not trim
the cases at this edge. The selection can be reversed, which means that
only the trimmed events at the start and end of cases are retained. As
such, this argument allows to cut intermediate parts out of traces.

## Usage

``` r
filter_trim(
  log,
  start_activities = NULL,
  end_activities = NULL,
  reverse = FALSE
)

# S3 method for class 'eventlog'
filter_trim(
  log,
  start_activities = NULL,
  end_activities = NULL,
  reverse = FALSE
)

# S3 method for class 'grouped_eventlog'
filter_trim(
  log,
  start_activities = NULL,
  end_activities = NULL,
  reverse = FALSE
)

# S3 method for class 'activitylog'
filter_trim(
  log,
  start_activities = NULL,
  end_activities = NULL,
  reverse = FALSE
)

# S3 method for class 'grouped_activitylog'
filter_trim(
  log,
  start_activities = NULL,
  end_activities = NULL,
  reverse = FALSE
)

ifilter_trim(log)
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

- start_activities, end_activities:

  [`character`](https://rdrr.io/r/base/character.html) vector (default
  [`NULL`](https://rdrr.io/r/base/NULL.html)): A vector of activity
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

- `filter_trim(eventlog)`: Filters activity instances for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `filter_trim(grouped_eventlog)`: Filters activity instances for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `filter_trim(activitylog)`: Filters activity instances for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `filter_trim(grouped_activitylog)`: Filters activity instances for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## Functions

- `ifilter_trim()`: Filter interactively

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
[`filter_time_period()`](https://bupaverse.github.io/edeaR/reference/filter_time_period.md),
[`filter_trace()`](https://bupaverse.github.io/edeaR/reference/filter_trace.md),
[`filter_trace_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_trace_frequency.md),
[`filter_trace_length()`](https://bupaverse.github.io/edeaR/reference/filter_trace_length.md),
[`filter_trim_lifecycle()`](https://bupaverse.github.io/edeaR/reference/filter_trim_lifecycle.md)
