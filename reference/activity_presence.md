# Metric: Activity Presence

Calculates for each activity type in what percentage of cases it is
present.

## Usage

``` r
activity_presence(log, sort = TRUE)

# S3 method for class 'eventlog'
activity_presence(log, sort = TRUE)

# S3 method for class 'grouped_eventlog'
activity_presence(log, sort = TRUE)

# S3 method for class 'activitylog'
activity_presence(log, sort = TRUE)

# S3 method for class 'grouped_activitylog'
activity_presence(log, sort = TRUE)
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

- sort:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `TRUE`):
  Sort output on count. Only for levels with frequency count output.

## Details

An indication of variance can be the presence of the activities in the
different cases. This metric shows for each activity the absolute number
of cases in which each activity occurs together with its relative
presence.

## Methods (by class)

- `activity_presence(eventlog)`: Compute activity presence for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `activity_presence(grouped_eventlog)`: Compute activity presence for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `activity_presence(activitylog)`: Compute activity presence for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `activity_presence(grouped_activitylog)`: Compute activity presence
  for a
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

Other metrics:
[`activity_frequency()`](https://bupaverse.github.io/edeaR/reference/activity_frequency.md),
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
[`trace_coverage()`](https://bupaverse.github.io/edeaR/reference/trace_coverage.md),
[`trace_length()`](https://bupaverse.github.io/edeaR/reference/trace_length.md)

## Examples

``` r
if (FALSE) { # \dontrun{
data <- data.frame(case = rep("A",5),
activity_id = c("A","B","C","D","E"),
activity_instance_id = 1:5,
lifecycle_id = rep("complete",5),
timestamp = 1:5,
resource = rep("resource 1", 5))

log <- bupaR::eventlog(data,case_id = "case",
activity_id = "activity_id",
activity_instance_id = "activity_instance_id",
lifecycle_id = "lifecycle_id",
timestamp = "timestamp",
resource_id = "resource")

activity_presence(log)
} # }
```
