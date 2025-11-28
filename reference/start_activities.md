# Start Activities

Analyse the start activities in the process.

## Usage

``` r
start_activities(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'eventlog'
start_activities(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_eventlog'
start_activities(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'activitylog'
start_activities(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_activitylog'
start_activities(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
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
  `"case"`, `"activity"`, `"resource"`, or `"resource-activity"`. For
  more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and 'Details' below.

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort output on count. Only for levels with frequency count output.

## Details

Argument `level` has the following options:

- On `"log"` level, this metric shows the absolute and relative number
  of activities that are the first activity in one or more of the cases.

- On `"case"` level, this metric provides an overview of the start
  activity of each case.

- On `"activity"` level, this metric calculates for each activity the
  absolute and relative number of cases that start with this activity
  type. Similar to the
  [`end_activities`](https://bupaverse.github.io/edeaR/reference/end_activities.md)
  metric, the relative number is calculated as a portion of the number
  of cases, being the number of "opportunities" that an activity could
  be the start activity. The cumulative sum is added to have an insight
  in the number of activities that is required to cover a certain part
  of the total.

- On `"resource"` level, an overview of which resources execute the
  first activity per case are provided.

- On `"resource-activity"` level, this metric shows for each occurring
  resource-activity combination the absolute and relative number of
  times this resource executes this activity as an start activity in a
  case.

## Methods (by class)

- `start_activities(eventlog)`: Computes the start activities for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `start_activities(grouped_eventlog)`: Computes the start activities
  for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `start_activities(activitylog)`: Computes the start activities for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `start_activities(grouped_activitylog)`: Computes the start activities
  for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`end_activities`](https://bupaverse.github.io/edeaR/reference/end_activities.md)

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
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
