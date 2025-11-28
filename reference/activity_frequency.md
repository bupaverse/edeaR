# Activity Frequency

Provides summary statistics about the frequency of activity types at the
level of log, traces, cases, activity types.

## Usage

``` r
activity_frequency(
  log,
  level = c("log", "trace", "activity", "case"),
  sort = TRUE
)

# S3 method for class 'eventlog'
activity_frequency(
  log,
  level = c("log", "trace", "activity", "case"),
  sort = TRUE
)

# S3 method for class 'grouped_eventlog'
activity_frequency(
  log,
  level = c("log", "trace", "activity", "case"),
  sort = TRUE
)

# S3 method for class 'activitylog'
activity_frequency(
  log,
  level = c("log", "trace", "activity", "case"),
  sort = TRUE
)

# S3 method for class 'grouped_activitylog'
activity_frequency(
  log,
  level = c("log", "trace", "activity", "case"),
  sort = TRUE
)
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

- level:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"log"`): Level of granularity for the analysis: `"log"` (default),
  `"trace"`, `"case"`, or `"activity"`. For more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and 'Details' below.

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort output on count. Only for levels with frequency count output.

## Details

Argument `level` has the following options:

- At `log` level, this metric shows the summary statistics of the
  frequency of activities throughout the complete log.

- On `case` level, this metric shows the absolute and relative number of
  times the different activity types occur in each case. The absolute
  number shows the number of distinct activity types that occur in each
  of the cases. The relative number is calculated based on the total
  activity executions in the case.

- On `trace` level, this metric presents the absolute and relative
  number of times a specific activity type occurs in each trace.

- On `activity` level, this metric provides the absolute and relative
  frequency of a specific activity in the complete log.

## Methods (by class)

- `activity_frequency(eventlog)`: Computes the activity frequency for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `activity_frequency(grouped_eventlog)`: Computes the activity
  frequency for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `activity_frequency(activitylog)`: Computes the activity frequency for
  an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `activity_frequency(grouped_activitylog)`: Computes the activity
  frequency for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

Other metrics:
[`activity_presence()`](https://bupaverse.github.io/edeaR/reference/activity_presence.md),
[`end_activities()`](https://bupaverse.github.io/edeaR/reference/end_activities.md),
[`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md),
[`number_of_repetitions()`](https://bupaverse.github.io/edeaR/reference/number_of_repetitions.md),
[`number_of_selfloops()`](https://bupaverse.github.io/edeaR/reference/number_of_selfloops.md),
[`number_of_traces()`](https://bupaverse.github.io/edeaR/reference/number_of_traces.md),
[`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),
[`resource_frequency()`](https://bupaverse.github.io/edeaR/reference/resource_frequency.md),
[`resource_involvement()`](https://bupaverse.github.io/edeaR/reference/resource_involvement.md),
[`resource_specialisation()`](https://bupaverse.github.io/edeaR/reference/resource_specialisation.md),
[`start_activities()`](https://bupaverse.github.io/edeaR/reference/start_activities.md),
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
