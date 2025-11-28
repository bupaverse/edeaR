# Idle Time

Calculates the amount of time that no activity occurs.

## Usage

``` r
idle_time(
  log,
  level = c("log", "trace", "case", "resource", "flow"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE
)

# S3 method for class 'eventlog'
idle_time(
  log,
  level = c("log", "trace", "case", "resource", "flow"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE
)

# S3 method for class 'grouped_eventlog'
idle_time(
  log,
  level = c("log", "case", "trace", "resource", "flow"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE
)

# S3 method for class 'activitylog'
idle_time(
  log,
  level = c("log", "trace", "case", "resource", "flow"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
  sort = TRUE
)

# S3 method for class 'grouped_activitylog'
idle_time(
  log,
  level = c("log", "trace", "case", "resource", "flow"),
  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
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
  `"trace"`, `"case"`, or `"resource"`. For more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and **Details** below.

- units:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"auto"`): The time unit in which the throughput times should be
  reported. Should be one of the following values: `"auto"` (default),
  `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the
  `units` argument of
  [`difftime()`](https://rdrr.io/r/base/difftime.html).

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort by decreasing idle time. Only relevant for `"trace"` and
  `"resource"` `level`.

## Details

Argument `level` has the following options:

- At `"log"` level, the idle time metric provides an overview of summary
  statistics of the idle time per case, aggregated over the complete
  log.

- On `"trace"` level, the idle time metric provides an overview of the
  summary statistics of the idle time for each trace in the log.

- On `"case"` level, the idle time metric provides an overview of the
  total idle time per case

- On `"resource"` level, this metric can be used to get an insight in
  the amount of time each resource "wastes" during the process.

## Methods (by class)

- `idle_time(eventlog)`: Computes the idle time for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `idle_time(grouped_eventlog)`: Computes the idle time for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `idle_time(activitylog)`: Computes the idle time for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `idle_time(grouped_activitylog)`: Computes the idle time for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),[`processing_time()`](https://bupaverse.github.io/edeaR/reference/processing_time.md),[`difftime()`](https://rdrr.io/r/base/difftime.html)

Other metrics:
[`activity_frequency()`](https://bupaverse.github.io/edeaR/reference/activity_frequency.md),
[`activity_presence()`](https://bupaverse.github.io/edeaR/reference/activity_presence.md),
[`end_activities()`](https://bupaverse.github.io/edeaR/reference/end_activities.md),
[`number_of_repetitions()`](https://bupaverse.github.io/edeaR/reference/number_of_repetitions.md),
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
