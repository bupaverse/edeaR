# Processing Time

Provides summary statistics about the processing time of the process.

In contrast to the
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md)
of the cases in a log, the metrics concerning the active time or the
actual processing time provide summary statistics on the processing time
of events on the level of the complete log, the specific cases, traces,
the activities, and the resource-activity combinations.

## Usage

``` r
processing_time(
  log,
  level = c("log", "trace", "case", "activity", "resource", "resource-activity",
    "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'eventlog'
processing_time(
  log,
  level = c("log", "trace", "case", "activity", "resource", "resource-activity",
    "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'grouped_eventlog'
processing_time(
  log,
  level = c("log", "trace", "case", "activity", "resource", "resource-activity",
    "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'activitylog'
processing_time(
  log,
  level = c("log", "trace", "case", "activity", "resource", "resource-activity",
    "activity-instance"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE,
  work_schedule = NULL
)

# S3 method for class 'grouped_activitylog'
processing_time(
  log,
  level = c("log", "trace", "case", "activity", "resource", "resource-activity",
    "activity-instance"),
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
  `"trace"`, `"case"`, `"activity"`, `"resource"`, or
  `"resource-activity"`. For more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and **Details** below.

- units:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"auto"`): The time unit in which the processing times should be
  reported. Should be one of the following values: `"auto"` (default),
  `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the
  `units` argument of
  [`difftime()`](https://rdrr.io/r/base/difftime.html).

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort on decreasing processing time. For `"case"` `level` only.

- work_schedule:

  A schedule of working hours. If provided, only working hours are
  counted as processing time.

## Details

Argument `level` has the following options:

- At `"log"` level, this metric calculates the summary statistics of the
  actual processing time per case, summarised over the complete event
  log.

- On `"trace"` level, the summary statistics of processing time can be
  calculated for each possible sequence of activities that appears in
  the event log.

- On `"case"` level, a list of cases with their processing time are
  provided.

- On `"activity"` level, an overview of the average processing time -or
  the service time- of each activity can be calculated.

- At `"resource"` level, this metric calculates the processing time per
  resource.

- On `"resource-activity"` level, the efficiency of resources by looking
  at the combination of each resource with each activity can be
  investigated.

## Methods (by class)

- `processing_time(eventlog)`: Computes processing time for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `processing_time(grouped_eventlog)`: Computes processing time for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `processing_time(activitylog)`: Computes processing time for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `processing_time(grouped_activitylog)`: Computes processing time for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md),[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),[`difftime()`](https://rdrr.io/r/base/difftime.html)

Other metrics:
[`activity_frequency()`](https://bupaverse.github.io/edeaR/reference/activity_frequency.md),
[`activity_presence()`](https://bupaverse.github.io/edeaR/reference/activity_presence.md),
[`end_activities()`](https://bupaverse.github.io/edeaR/reference/end_activities.md),
[`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md),
[`number_of_repetitions()`](https://bupaverse.github.io/edeaR/reference/number_of_repetitions.md),
[`number_of_selfloops()`](https://bupaverse.github.io/edeaR/reference/number_of_selfloops.md),
[`number_of_traces()`](https://bupaverse.github.io/edeaR/reference/number_of_traces.md),
[`resource_frequency()`](https://bupaverse.github.io/edeaR/reference/resource_frequency.md),
[`resource_involvement()`](https://bupaverse.github.io/edeaR/reference/resource_involvement.md),
[`resource_specialisation()`](https://bupaverse.github.io/edeaR/reference/resource_specialisation.md),
[`start_activities()`](https://bupaverse.github.io/edeaR/reference/start_activities.md),
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
