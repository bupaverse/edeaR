# Number of Self-loops

Provides information statistics on the number of self-loops in a trace.

Activity instances of the same activity type that are executed more than
once immediately after each other by the same resource are in a
self-loop ("length-1-loop"). If an activity instance of the same
activity type is executed 3 times after each other by the same resource,
this is defined as a "size 2 self-loop".

## Usage

``` r
number_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'eventlog'
number_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_eventlog'
number_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'activitylog'
number_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_activitylog'
number_of_selfloops(
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

Two types of self-loops are defined, which can be chosen using the
`type` argument:

- `"repeat"` self-loops are activity executions of the same activity
  type that are executed immediately following each other by the same
  resource.

- `"redo"` self-loops are activity executions of the same activity type
  that are executed immediately following each other by a different
  resource.

Argument `level` has the following options:

- At `"log"` level, the summary statistics of the number of self-loops
  within a trace can give a first insight in the amount of waste in a
  log. As stated earlier, each combination of two occurrences of the
  same activity executed by the same resource will be counted as one
  repeat self-loop of this activity.

- On `"case"` level, an overview is provided of the absolute and
  relative number of repeat and redo self-loops in each case. To
  calculate the relative number, each (repeat or redo) self-loop is
  counted as 1 occurrence, and the other activity instances are also
  counted as 1.

- On `"activity"` level, the absolute and relative number of self-loops
  per activity can be an indication for which activities are causing the
  most waste in the process.

- On `"resource"` level, this metric can give insights into which
  resources needs to repeat their work most often within a case, or for
  which resource the work they did should be redone by another resource
  within the same case. This metric shows the absolute and relative
  number of both repeat and redo self-loops for each resource in the
  log.

- On `"resource-activity"` level, this metric can be used to get an
  insight in which activities are the most crucial for which resources.
  This metric shows the absolute and relative number of both repeat and
  redo self-loops for each of the resource-activity combinations that
  occur in the log. Two different relative numbers are provided here,
  one from the resource perspective and one from the activity
  perspective. At the resource perspective, the denominator is the total
  number of executions by the resource under consideration. At the
  activity perspective, the denominator is the total number of
  occurrences of the activity under consideration.

## Methods (by class)

- `number_of_selfloops(eventlog)`: Computes the number of self-loops for
  an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `number_of_selfloops(grouped_eventlog)`: Computes the number of
  self-loops for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `number_of_selfloops(activitylog)`: Computes the number of self-loops
  for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `number_of_selfloops(grouped_activitylog)`: Computes the number of
  self-loops for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`number_of_repetitions`](https://bupaverse.github.io/edeaR/reference/number_of_repetitions.md)

Other metrics:
[`activity_frequency()`](https://bupaverse.github.io/edeaR/reference/activity_frequency.md),
[`activity_presence()`](https://bupaverse.github.io/edeaR/reference/activity_presence.md),
[`end_activities()`](https://bupaverse.github.io/edeaR/reference/end_activities.md),
[`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md),
[`number_of_repetitions()`](https://bupaverse.github.io/edeaR/reference/number_of_repetitions.md),
[`number_of_traces()`](https://bupaverse.github.io/edeaR/reference/number_of_traces.md),
[`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),
[`resource_frequency()`](https://bupaverse.github.io/edeaR/reference/resource_frequency.md),
[`resource_involvement()`](https://bupaverse.github.io/edeaR/reference/resource_involvement.md),
[`resource_specialisation()`](https://bupaverse.github.io/edeaR/reference/resource_specialisation.md),
[`start_activities()`](https://bupaverse.github.io/edeaR/reference/start_activities.md),
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
