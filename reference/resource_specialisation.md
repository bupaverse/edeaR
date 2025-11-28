# Resource Specialisation

Analyses whether resources specialise in specific activities.

This metric can give an overview of which resources are performing
certain activities more than others, and which resources are responsible
for containing all knowledge or capabilities on one topic.

## Usage

``` r
resource_specialisation(
  log,
  level = c("log", "activity", "resource"),
  sort = TRUE
)

resource_specialization(
  log,
  level = c("log", "activity", "resource"),
  sort = TRUE
)

# S3 method for class 'log'
resource_specialisation(
  log,
  level = c("log", "activity", "resource"),
  sort = TRUE
)

# S3 method for class 'grouped_log'
resource_specialisation(
  log,
  level = c("log", "activity", "resource"),
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
  `"log"`): Level of granularity for the analysis: `"log"` (default), ,
  `"activity"`, or `"resource"`. For more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and 'Details' below.

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort output on count. Only for levels with frequency count output.

## Details

Argument `level` has the following options:

- At `"log"` level, this metric provides summary statistics on the
  number of distinct activities executed per resource.

- On `"activity"` level, this metric provides an overview of the
  absolute and relative number of different resources executing this
  activity within the complete log. This will give insights into which
  activities resources are specialised in.

- On `"resource"` level, this metric shows the absolute and relative
  number of distinct activities that each resource executes.

## Methods (by class)

- `resource_specialisation(log)`: Computes the resource specialisation
  for a [`log`](https://bupaverse.github.io/bupaR/reference/log.html).

- `resource_specialisation(grouped_log)`: Computes the resource
  specialisation for a
  [`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

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
[`start_activities()`](https://bupaverse.github.io/edeaR/reference/start_activities.md),
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
