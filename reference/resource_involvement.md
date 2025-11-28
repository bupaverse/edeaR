# Resource Involvement

Calculates for each resource or resource-activity combination in what
percentage of cases it is present.

Next to the
[`resource_frequency`](https://bupaverse.github.io/edeaR/reference/resource_frequency.md),
the involvement of resources in cases can be of interest to, e.g.,
decide how "indispensable" they are. This metric is provided on three
levels of analysis, which are the cases, the resources, and the
resource-activity combinations.

## Usage

``` r
resource_involvement(
  log,
  level = c("case", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'log'
resource_involvement(
  log,
  level = c("case", "resource", "resource-activity"),
  sort = TRUE
)

# S3 method for class 'grouped_log'
resource_involvement(
  log,
  level = c("case", "resource", "resource-activity"),
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
  `"case"`): Level of granularity for the analysis: `"case"` (default),
  `"resource"`, or `"resource-activity"`. For more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and 'Details' below.

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort output on count. Only for levels with frequency count output.

## Details

Argument `level` has the following options:

- On `"case"` level, the absolute and relative number of distinct
  resources executing activities in each case is calculated, to get an
  overview of which cases are handled by a small amount of resources and
  which cases need more resources, indicating a higher level of variance
  in the process.

- On `"resource"` level, this metric provides the absolute and relative
  number of cases in which each resource is involved, indicating which
  resources are more "necessary" within the process than the others.

- On `"resource-activity"` level, this metric provides a list of all
  resource-activity combinations with the absolute and relative number
  of cases in which each resource-activity combination is involved.

## Methods (by class)

- `resource_involvement(log)`: Computes the resource involvement for a
  [`log`](https://bupaverse.github.io/bupaR/reference/log.html).

- `resource_involvement(grouped_log)`: Computes the resource involvement
  for a
  [`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`resource_frequency`](https://bupaverse.github.io/edeaR/reference/resource_frequency.md)

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
[`resource_specialisation()`](https://bupaverse.github.io/edeaR/reference/resource_specialisation.md),
[`start_activities()`](https://bupaverse.github.io/edeaR/reference/start_activities.md),
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
