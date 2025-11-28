# Number of Repetitions

Provides information statistics on the number of repetitions

A repetition is an execution of an activity within a case while that
activity has already been executed before, but one or more other
activities are executed in between.

## Usage

``` r
number_of_repetitions(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'eventlog'
number_of_repetitions(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_eventlog'
number_of_repetitions(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'activitylog'
number_of_repetitions(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_activitylog'
number_of_repetitions(
  log,
  type = c("all", "repeat", "redo"),
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

- type:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"all"`): The type of repetitions: `"all"` (default), `"repeat"`, or
  `"redo"`. For more information, see 'Details' below.

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

- At `"log"` level, this metric shows the summary statistics of the
  number of repetitions within a case, which can provide insights in the
  amount of waste in a log. Each combination of two or more occurrences
  of the same activity, executed not immediately following each other,
  by the same resource is counted as one repeat repetition of this
  activity.

- On `"case"` level, this metric provides the absolute and relative
  number of repetitions in each case.

- On `"activity"` level, this metric shows which activities occur the
  most in a repetition. The absolute and relative number of both repeat
  and redo repetitions is provided by this metric, giving an overview
  per activity.

- On `"resource"` level, it can be interesting to have an overview of
  which resources need more than one time to execute an activity in a
  case or which resources need to have an activity redone later on in
  the case by another resource. This metric provides the absolute and
  relative number of times each resource appears in a repetition.

- On `"resource-activity"` level, this metric provides specific
  information about which activities and which resources are involved in
  the repetitions. For this metric the absolute and relative number of
  repeat and redo repetitions is provided. Again, two difierent relative
  numbers are provided, one relative to the total number of executions
  of the activity in the complete log, and one relative to the total
  number of executions performed by the resource throughout the complete
  log.

Similar to the
[self-loop](https://bupaverse.github.io/edeaR/reference/number_of_selfloops.md)
metric, a distinction should be made between `"repeat"` and `"redo"`
repetitions, as can be set by the `type` argument:

- `"repeat"` repetitions are activity executions of the same activity
  type that are executed not immediately following each other, but by
  the same resource.

- `"redo"` repetitions are activity executions of the same activity type
  that are executed not immediately following each other and by a
  different resource than the first activity occurrence of this activity
  type.

## Methods (by class)

- `number_of_repetitions(eventlog)`: Computes the number of repetitions
  for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `number_of_repetitions(grouped_eventlog)`: Computes the number of
  repetitions for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `number_of_repetitions(activitylog)`: Computes the number of
  repetitions for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `number_of_repetitions(grouped_activitylog)`: Computes the number of
  repetitions for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`number_of_selfloops`](https://bupaverse.github.io/edeaR/reference/number_of_selfloops.md)

Other metrics:
[`activity_frequency()`](https://bupaverse.github.io/edeaR/reference/activity_frequency.md),
[`activity_presence()`](https://bupaverse.github.io/edeaR/reference/activity_presence.md),
[`end_activities()`](https://bupaverse.github.io/edeaR/reference/end_activities.md),
[`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md),
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
