# Trace Length

Analysis of trace lengths

This metric provides an overview of the number of activities that occur
in each trace.

An important remark is that this metric takes into account each instance
of an activity, but not the individual lifecycle events.

## Usage

``` r
trace_length(log, level = c("log", "trace", "case"), sort = TRUE)

# S3 method for class 'eventlog'
trace_length(log, level = c("log", "trace", "case"), sort = TRUE)

# S3 method for class 'grouped_eventlog'
trace_length(log, level = c("log", "trace", "case"), sort = TRUE)

# S3 method for class 'activitylog'
trace_length(log, level = c("log", "trace", "case"), sort = TRUE)

# S3 method for class 'grouped_activitylog'
trace_length(log, level = c("log", "trace", "case"), sort = TRUE)
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

- At `"log"` level, the summary statistics describing the trace length
  of cases in an aggregated fashion.

- On `"trace"` level, the trace length of the different process variants
  or traces in the log are calculated.

- On `"case"` level, the trace lengths for each case are computed.

## Methods (by class)

- `trace_length(eventlog)`: Computes trace length for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `trace_length(grouped_eventlog)`: Computes trace length for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `trace_length(activitylog)`: Computes trace length for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `trace_length(grouped_activitylog)`: Computes trace length for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

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
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md)
