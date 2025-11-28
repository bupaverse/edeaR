# Metric: Size of repetitions

Provides summary statistics on the sizes of repetitions.

## Usage

``` r
size_of_repetitions(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
)

# S3 method for class 'eventlog'
size_of_repetitions(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
)

# S3 method for class 'grouped_eventlog'
size_of_repetitions(
  log,
  type = c("repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
)

# S3 method for class 'activitylog'
size_of_repetitions(
  log,
  type = c("repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
)

# S3 method for class 'grouped_activitylog'
size_of_repetitions(
  log,
  type = c("repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
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

## Methods (by class)

- `size_of_repetitions(eventlog)`: Size of repetitions for eventlog

- `size_of_repetitions(grouped_eventlog)`: Size of repetitions for
  grouped event log

- `size_of_repetitions(activitylog)`: Size of repetitions for
  activitylog

- `size_of_repetitions(grouped_activitylog)`: Size of repetitions for
  grouped activitylog

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`number_of_repetitions`](https://bupaverse.github.io/edeaR/reference/number_of_repetitions.md)
