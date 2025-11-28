# Filter Precedence Relations with Identical Resources

Filters cases based on the precedence relations between two sets of
activities, where both antecendent and consequent have to be executed by
the same resource.

## Usage

``` r
filter_precedence_resource(
  log,
  antecedents,
  consequents,
  precedence_type = c("directly_follows", "eventually_follows"),
  filter_method = c("all", "one_of", "none"),
  reverse = FALSE
)

# S3 method for class 'eventlog'
filter_precedence_resource(
  log,
  antecedents,
  consequents,
  precedence_type = c("directly_follows", "eventually_follows"),
  filter_method = c("all", "one_of", "none"),
  reverse = FALSE
)

# S3 method for class 'activitylog'
filter_precedence_resource(
  log,
  antecedents,
  consequents,
  precedence_type = c("directly_follows", "eventually_follows"),
  filter_method = c("all", "one_of", "none"),
  reverse = FALSE
)

# S3 method for class 'grouped_log'
filter_precedence_resource(
  log,
  antecedents,
  consequents,
  precedence_type = c("directly_follows", "eventually_follows"),
  filter_method = c("all", "one_of", "none"),
  reverse = FALSE
)

ifilter_precedence_resource(log)
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

- antecedents, consequents:

  [`character`](https://rdrr.io/r/base/character.html) vector: The set
  of antecendent and consequent activities. Both are
  [`character`](https://rdrr.io/r/base/character.html) vectors
  containing at least one activity identifier. All pairs of antecedents
  and consequents are turned into seperate precedence rules.

- precedence_type:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"directly_follows"`): When `"directly_follows"`, the consequent
  activity should happen immediately after the antecedent activities.  
  When `"eventually_follows"`, other events are allowed to happen in
  between.

- filter_method:

  [`character`](https://rdrr.io/r/base/character.html) (default
  `"all"`): When `"all"`, only cases where all the relations are valid
  are preserved.  
  When `"one_of"`, all the cases where at least one of the conditions
  hold, are preserved.  
  When `"none"`, none of the relations are allowed.

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

## Methods (by class)

- `filter_precedence_resource(eventlog)`: Filters cases for an
  [`eventlog`](https://bupaverse.github.io/bupaR/reference/eventlog.html).

- `filter_precedence_resource(activitylog)`: Filters cases for an
  [`activitylog`](https://bupaverse.github.io/bupaR/reference/activitylog.html).

- `filter_precedence_resource(grouped_log)`: Filters cases for a
  [`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html).

## Functions

- `ifilter_precedence_resource()`: Filter interactively

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

[`filter_precedence()`](https://bupaverse.github.io/edeaR/reference/filter_precedence.md)

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
[`filter_lifecycle_presence()`](https://bupaverse.github.io/edeaR/reference/filter_lifecycle_presence.md),
[`filter_precedence()`](https://bupaverse.github.io/edeaR/reference/filter_precedence.md),
[`filter_precedence_condition()`](https://bupaverse.github.io/edeaR/reference/filter_precedence_condition.md),
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
