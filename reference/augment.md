# Augment Log

Augment log with results from metric computation.

## Usage

``` r
augment(metric, log, columns, prefix = "")

# S3 method for class 'log_metric'
augment(metric, log, columns, prefix = "")

# S3 method for class 'case_metric'
augment(metric, log, columns, prefix = "")

# S3 method for class 'activity_metric'
augment(metric, log, columns, prefix = "")

# S3 method for class 'resource_metric'
augment(metric, log, columns, prefix = "")

# S3 method for class 'resource_activity_metric'
augment(metric, log, columns, prefix = "")

# S3 method for class 'trace_metric'
augment(metric, log, columns, prefix = "")
```

## Arguments

- metric:

  Metric computed by edeaR

- log:

  [`log`](https://bupaverse.github.io/bupaR/reference/log.html): Object
  of class [`log`](https://bupaverse.github.io/bupaR/reference/log.html)
  or derivatives
  ([`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html),
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html),
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html),
  etc.) that was used to compute the `metric`.

- columns:

  [`character`](https://rdrr.io/r/base/character.html) vector: Column
  names from the `metric` that you want to add to the `log`. If missing,
  defaults to all columns.

- prefix:

  [`character`](https://rdrr.io/r/base/character.html): Prefix to be
  added to the newly added metric columns in the `log`.

## Value

Object of class
[`log`](https://bupaverse.github.io/bupaR/reference/log.html) or
derivatives
([`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html),
[`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html),
[`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html),
etc.). Same class as the `log` input.

## Methods (by class)

- `augment(log_metric)`: Augment log metric

- `augment(case_metric)`: Augment case metric

- `augment(activity_metric)`: Augment activity metric

- `augment(resource_metric)`: Augment resource metric

- `augment(resource_activity_metric)`: Augment resource-activity metric

- `augment(trace_metric)`: Augment trace metric

## Examples

``` r
if (FALSE) { # \dontrun{
sepsis %>%
  throughput_time("case") %>%
  augment(sepsis)
} # }
```
