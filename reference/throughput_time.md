# Throughput Time of Cases

Provides summary statistics concerning the throughput times of cases.

## Usage

``` r
throughput_time(
  log,
  level = c("log", "trace", "case", "activity", "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'eventlog'
throughput_time(
  log,
  level = c("log", "trace", "case", "activity", "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'grouped_eventlog'
throughput_time(
  log,
  level = c("log", "trace", "case", "activity", "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'activitylog'
throughput_time(
  log,
  level = c("log", "trace", "case", "activity", "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'grouped_activitylog'
throughput_time(
  log,
  level = c("log", "trace", "case", "activity", "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
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
  `"trace"`, or `"case"`. For more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and **Details** below.

- units:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"auto"`): The time unit in which the throughput times should be
  reported. Should be one of the following values: `"auto"` (default),
  `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the
  `units` argument of
  [`difftime`](https://rdrr.io/r/base/difftime.html).

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort output on count. Only for levels with frequency count output.

- work_schedule:

  A schedule of working hours. If provided, only working hours are
  counted as processing time.

## Details

Argument `level` has the following options:

- At `"log"` level, the summary statistics describing the throughput
  time of cases in an aggregated fashion.

- On `"trace"` level, the throughput time of the different process
  variants or traces in the log are calculated.

- On `"case"` level, the throughput time is defined as the total
  duration of the case, or the difference between the timestamp of the
  end event and the timestamp of the start event of the case. Possible
  [`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md)
  is also included in this calculation.

- On `"activity-instance"` level, the throughput time of each activity
  instance. Throughput here is defined as the difference between the
  first and last event, without considering the lifecycle status. For
  the lifecycle-aware throughput time (e.g. not incorporating the time
  the activity is "suspended"), see processing time.

- on `'activity` level, summary statistics describing the throuhgput
  time of activity instances (see above) per activity type. For other
  levels (e.g. `"activity"`, `"resource"`, or `"resource-activity"`),
  the throughput time is equal to the
  [`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md)
  and are, therefore, not supported by this method.

## Methods (by class)

- `throughput_time(eventlog)`: Computes throughput time for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `throughput_time(grouped_eventlog)`: Computes throughput time for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `throughput_time(activitylog)`: Computes throughput time for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `throughput_time(grouped_activitylog)`: Computes throughput time for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md),[`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),[`difftime()`](https://rdrr.io/r/base/difftime.html)

Other metrics:
[`activity_frequency()`](https://bupaverse.github.io/edeaR/reference/activity_frequency.md),
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
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
