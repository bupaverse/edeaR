# Trace Coverage

Analyses the structuredness of a log by use of trace frequencies.

## Usage

``` r
trace_coverage(log, level = c("log", "trace", "case"), sort = TRUE)

# S3 method for class 'log'
trace_coverage(log, level = c("log", "trace", "case"), sort = TRUE)

# S3 method for class 'grouped_log'
trace_coverage(log, level = c("log", "trace", "case"), sort = TRUE)
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
  `"trace"`, or `"case"`. For more information, see
  [`vignette("metrics", "edeaR")`](https://bupaverse.github.io/edeaR/articles/metrics.md)
  and **Details** below.

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort output on count. Only for levels with frequency count output.

## Details

Argument `level` has the following options:

- At `"log"` level, summary statistics of the coverage of traces are
  returned.

- On `"trace"` level, the absolute and relative frequency of each trace
  are returned.

- On `"case"` level, the coverage of the corresponding trace is returned
  for each case.

## Methods (by class)

- `trace_coverage(log)`: Calculates trace coverage metric for a
  [`log`](https://bupaverse.github.io/bupaR/reference/log.html).

- `trace_coverage(grouped_log)`: Calculates trace coverage metric for a
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
[`resource_specialisation()`](https://bupaverse.github.io/edeaR/reference/resource_specialisation.md),
[`start_activities()`](https://bupaverse.github.io/edeaR/reference/start_activities.md),
[`throughput_time()`](https://bupaverse.github.io/edeaR/reference/throughput_time.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)
