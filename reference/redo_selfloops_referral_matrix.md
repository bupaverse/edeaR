# Referral matrix selfloops

Provides a list of initatiors and completers of redo selfloops

## Usage

``` r
redo_selfloops_referral_matrix(log)

# S3 method for class 'eventlog'
redo_selfloops_referral_matrix(log)

# S3 method for class 'activitylog'
redo_selfloops_referral_matrix(log)
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

## Methods (by class)

- `redo_selfloops_referral_matrix(eventlog)`: Compute matrix for
  eventlog

- `redo_selfloops_referral_matrix(activitylog)`: Compute matrix for
  activitylog

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`number_of_selfloops`](https://bupaverse.github.io/edeaR/reference/number_of_selfloops.md)
