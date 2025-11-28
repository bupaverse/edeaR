# Calculate queuing length

**\[experimental\]**

## Usage

``` r
calculate_queuing_length(
  queueing_times,
  level = c("log", "activity", "resource"),
  time_interval
)
```

## Arguments

- queueing_times:

  Object of class `queuing_times`, returned by
  [`calculate_queuing_times`](https://bupaverse.github.io/edeaR/reference/calculate_queuing_times.md).

- level:

  [`character`](https://rdrr.io/r/base/character.html) (default "log"):
  Level of granularity for the analysis: `"log"`, `"activity"`,
  `"resource"`. For more information, see 'Details' below.

- time_interval:

  The time interval after which the queue length should be calculated.
  For more information, see 'Details' below and the `by` argument of
  [`seq.Date`](https://rdrr.io/r/base/seq.Date.html).

## Details

Argument `level` has the following options:

- At `log` level, this metric calculates the total number of activity
  instances that are queued at a given moment in time.

- At `resource` level, this metric calculates the total number activity
  instances that are queued for a given resource.

- On `activity` level, this metric calculates the total number of
  activity instances that are queue for a given activity type.

Argument `time_interval` has the following options (see also the `by`
argument of [`seq.Date`](https://rdrr.io/r/base/seq.Date.html)):

- A [`numeric`](https://rdrr.io/r/base/numeric.html) as number of days.

- An object of class [`difftime`](https://rdrr.io/r/base/difftime.html).

- A [`character`](https://rdrr.io/r/base/character.html) string, which
  could be one of `"day"`, `"week"`, `"month"`, `"quarter"`, or
  `"year"`. The first day for which queue length is calculated, is the
  first timestamp found in the log.

## See also

[`calculate_queuing_times`](https://bupaverse.github.io/edeaR/reference/calculate_queuing_times.md),
[`seq.Date`](https://rdrr.io/r/base/seq.Date.html)
