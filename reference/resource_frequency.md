# Resource Frequency

Analyses the frequency of resources at different levels of analysis.

## Usage

``` r
resource_frequency(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'eventlog'
resource_frequency(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_eventlog'
resource_frequency(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'activitylog'
resource_frequency(
  log,
  level = c("log", "case", "activity", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_activitylog'
resource_frequency(
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

- At `"log"` level, summary statistics show the number of times a
  resource executes an activity in the complete log.

- On `"case"` level, summary statistics of the frequency of resources
  can be used to get a better view on the variance between the different
  cases, to get an insight into the number of different resources
  working on each case together with the number of activities a resource
  executes per case.

- On `"activity"` level, the resource frequency states how many
  different resources are executing a specific activity in the complete
  log.

- On `"resource"` level, this metric simply shows the absolute and
  relative frequency of occurrences of each resource in the complete
  log.

- On `"resource-activity"` level, the absolute and relative number of
  times each resource-activity combination occurs in the complete log
  can be calculated. Two different relative numbers are provided here,
  one from the resource perspective and one from the activity
  perspective. At the resource perspective, the denominator is the total
  number of executions by the resource under consideration. At the
  activity perspective, the denominator is the total number of
  occurrences of the activity under consideration.

## Methods (by class)

- `resource_frequency(eventlog)`: Computes the resource frequency for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `resource_frequency(grouped_eventlog)`: Computes the resource
  frequency for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `resource_frequency(activitylog)`: Computes the resource frequency for
  an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `resource_frequency(grouped_activitylog)`: Computes the resource
  frequency for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`resource_involvement`](https://bupaverse.github.io/edeaR/reference/resource_involvement.md)

Other metrics:
[`activity_frequency()`](https://bupaverse.github.io/edeaR/reference/activity_frequency.md),
[`activity_presence()`](https://bupaverse.github.io/edeaR/reference/activity_presence.md),
[`end_activities()`](https://bupaverse.github.io/edeaR/reference/end_activities.md),
[`idle_time()`](https://bupaverse.github.io/edeaR/reference/idle_time.md),
[`number_of_repetitions()`](https://bupaverse.github.io/edeaR/reference/number_of_repetitions.md),
[`number_of_selfloops()`](https://bupaverse.github.io/edeaR/reference/number_of_selfloops.md),
[`number_of_traces()`](https://bupaverse.github.io/edeaR/reference/number_of_traces.md),
[`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),
[`resource_involvement()`](https://bupaverse.github.io/edeaR/reference/resource_involvement.md),
[`resource_specialisation()`](https://bupaverse.github.io/edeaR/reference/resource_specialisation.md),
[`start_activities()`](https://bupaverse.github.io/edeaR/reference/start_activities.md),
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
