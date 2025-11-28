# Filter Life Cycle Presence

Filters activity instances based on the presence (or absence) of life
cycles.

## Usage

``` r
filter_lifecycle_presence(
  log,
  lifecycles,
  method = c("all", "none", "one_of", "exact", "only"),
  reverse = FALSE
)

# S3 method for class 'eventlog'
filter_lifecycle_presence(
  log,
  lifecycles,
  method = c("all", "none", "one_of", "exact", "only"),
  reverse = FALSE
)

# S3 method for class 'grouped_eventlog'
filter_lifecycle_presence(
  log,
  lifecycles,
  method = c("all", "none", "one_of", "exact", "only"),
  reverse = FALSE
)

ifilter_lifecycle_presence(log)
```

## Arguments

- log:

  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html):
  Object of class
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html)
  or derivatives
  ([`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html)).

- lifecycles:

  [`character`](https://rdrr.io/r/base/character.html) vector: A vector
  of life cycle identifiers.

- method:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"all"`): Filter method: `"all"` (default), `"none"`, `"one_of"`,
  `"exact"`, or `"only"`. For more information, see **Details** below.

- reverse:

  [`logical`](https://rdrr.io/r/base/logical.html) (default `FALSE`):
  Indicating whether the selection should be reversed.

## Value

When given an object of type
[`log`](https://bupaverse.github.io/bupaR/reference/log.html), it will
return a filtered
[`log`](https://bupaverse.github.io/bupaR/reference/log.html). When
given an object of type
[`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html),
the filter will be applied in a stratified way (i.e. each separately for
each group). The returned log will be grouped on the same variables as
the original log.

## Details

This function allows to filter activity instances that (do not) contain
certain life cycle identifiers. It requires as input a vector containing
one or more life cycle labels and it has a `method` argument with
following options:

- `"all"` means that all the specified life cycle labels must be present
  for an activity instance to be selected.

- `"none"` means that they are not allowed to be present.

- `"one_of"` means that at least one of them must be present.

- `"exact"` means that only exactly these life cycle labels can be
  present (although multiple times and in random orderings).

- `"only"` means that only (a set of) these life cycle labels are
  allowed to be present.

## Methods (by class)

- `filter_lifecycle_presence(eventlog)`: Filters activity instances on
  the presence of life cycle labels for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `filter_lifecycle_presence(grouped_eventlog)`: Filters activity
  instances on the presence of life cycle labels for a
  [`grouped_eventlog`](https://bupaverse.github.io/bupaR/reference/grouped_eventlog.html).

## Functions

- `ifilter_lifecycle_presence()`: Filter interactively

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`lifecycle_id`](https://bupaverse.github.io/bupaR/reference/lifecycle_id.html)

Other filters:
[`filter_activity()`](https://bupaverse.github.io/edeaR/reference/filter_activity.md),
[`filter_activity_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_activity_frequency.md),
[`filter_activity_instance()`](https://bupaverse.github.io/edeaR/reference/filter_activity_instance.md),
[`filter_activity_presence()`](https://bupaverse.github.io/edeaR/reference/filter_activity_presence.md),
[`filter_case()`](https://bupaverse.github.io/edeaR/reference/filter_case.md),
[`filter_case_condition()`](https://bupaverse.github.io/edeaR/reference/filter_case_condition.md),
[`filter_endpoints()`](https://bupaverse.github.io/edeaR/reference/filter_endpoints.md),
[`filter_endpoints_condition()`](https://bupaverse.github.io/edeaR/reference/filter_endpoints_condition.md),
[`filter_flow_time()`](https://bupaverse.github.io/edeaR/reference/filter_flow_time.md),
[`filter_idle_time()`](https://bupaverse.github.io/edeaR/reference/filter_idle_time.md),
[`filter_infrequent_flows()`](https://bupaverse.github.io/edeaR/reference/filter_infrequent_flows.md),
[`filter_lifecycle()`](https://bupaverse.github.io/edeaR/reference/filter_lifecycle.md),
[`filter_precedence()`](https://bupaverse.github.io/edeaR/reference/filter_precedence.md),
[`filter_precedence_condition()`](https://bupaverse.github.io/edeaR/reference/filter_precedence_condition.md),
[`filter_precedence_resource()`](https://bupaverse.github.io/edeaR/reference/filter_precedence_resource.md),
[`filter_processing_time()`](https://bupaverse.github.io/edeaR/reference/filter_processing_time.md),
[`filter_resource()`](https://bupaverse.github.io/edeaR/reference/filter_resource.md),
[`filter_resource_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_resource_frequency.md),
[`filter_throughput_time()`](https://bupaverse.github.io/edeaR/reference/filter_throughput_time.md),
[`filter_time_period()`](https://bupaverse.github.io/edeaR/reference/filter_time_period.md),
[`filter_trace()`](https://bupaverse.github.io/edeaR/reference/filter_trace.md),
[`filter_trace_frequency()`](https://bupaverse.github.io/edeaR/reference/filter_trace_frequency.md),
[`filter_trace_length()`](https://bupaverse.github.io/edeaR/reference/filter_trace_length.md),
[`filter_trim()`](https://bupaverse.github.io/edeaR/reference/filter_trim.md),
[`filter_trim_lifecycle()`](https://bupaverse.github.io/edeaR/reference/filter_trim_lifecycle.md)
