# Calculate queuing times

**\[experimental\]**

## Usage

``` r
calculate_queuing_times(
  log,
  units = c("auto", "secs", "mins", "hours", "days", "weeks")
)

# S3 method for class 'eventlog'
calculate_queuing_times(
  log,
  units = c("auto", "secs", "mins", "hours", "days", "weeks")
)

# S3 method for class 'activitylog'
calculate_queuing_times(
  log,
  units = c("auto", "secs", "mins", "hours", "days", "weeks")
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

- units:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"auto"`): The time unit in which the throughput times should be
  reported. Should be one of the following values: `"auto"` (default),
  `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the
  `units` argument of
  [`difftime`](https://rdrr.io/r/base/difftime.html).

## Value

Returns a list of all the activity instances, with the time they
started, and the time since they were queued. Notice that this does not
take into account any process model notion! The time since they are
queued is the completion time of the previous activity in the log.

## Methods (by class)

- `calculate_queuing_times(eventlog)`: Calculate queueing times for
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html)
  and
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

- `calculate_queuing_times(activitylog)`: Calculate queueing times for
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html)
  and
  [`grouped_activitylog`](https://bupaverse.github.io/bupaR/reference/grouped_activitylog.html).

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`difftime`](https://rdrr.io/r/base/difftime.html)
