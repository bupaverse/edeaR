# Metric: Size of selfloops

Provides summary statistics on the sizes of selfloops

## Usage

``` r
size_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
)

# S3 method for class 'eventlog'
size_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
)

# S3 method for class 'grouped_eventlog'
size_of_selfloops(
  log,
  type = c("repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-acitivty")
)

# S3 method for class 'activitylog'
size_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
  level = c("log", "case", "activity", "resource", "resource-activity")
)

# S3 method for class 'grouped_activitylog'
size_of_selfloops(
  log,
  type = c("all", "repeat", "redo"),
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

- `size_of_selfloops(eventlog)`: Size of selfloops for eventlog

- `size_of_selfloops(grouped_eventlog)`: Size of selfloops for grouped
  eventlog

- `size_of_selfloops(activitylog)`: Size of selfloops for activitylog

- `size_of_selfloops(grouped_activitylog)`: Size of selfloops for
  grouped activitylog

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`number_of_selfloops`](https://bupaverse.github.io/edeaR/reference/number_of_selfloops.md)
