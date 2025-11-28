# Filter Precedence Relations

Filters cases based on the precedence relations between two sets of
activities.

## Usage

``` r
filter_precedence(
  log,
  antecedents,
  consequents,
  precedence_type = c("directly_follows", "eventually_follows"),
  filter_method = c("all", "one_of", "none"),
  reverse = FALSE
)

# S3 method for class 'log'
filter_precedence(
  log,
  antecedents,
  consequents,
  precedence_type = c("directly_follows", "eventually_follows"),
  filter_method = c("all", "one_of", "none"),
  reverse = FALSE
)

# S3 method for class 'grouped_log'
filter_precedence(
  log,
  antecedents,
  consequents,
  precedence_type = c("directly_follows", "eventually_follows"),
  filter_method = c("all", "one_of", "none"),
  reverse = FALSE
)

ifilter_precedence(log)
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

## Details

In order to extract a subset of an event log which conforms with a set
of precedence rules, one can use the `filter_precedence` method. There
are two types of precendence relations which can be tested: activities
that should directly follow (`"directly_follows"`) each other, or
activities that should eventually follow (`"eventually_follows"`) each
other. The type can be set with the `precedence_type` argument.

Further, the filter requires a vector of one or more `antecedents`
(containing activity labels), and one or more `consequents`.

Finally, a `filter_method` argument can be set. This argument is
relevant when there is more than one antecedent or consequent. In such a
case, you can specify that all possible precedence combinations must be
present (`"all"`), at least one of them (`"one_of"`), or none
(`"none"`).

## Methods (by class)

- `filter_precedence(log)`: Filters cases for a
  [`log`](https://bupaverse.github.io/bupaR/reference/log.html).

- `filter_precedence(grouped_log)`: Filters cases for a
  [`grouped_log`](https://bupaverse.github.io/bupaR/reference/grouped_log.html).

## Functions

- `ifilter_precedence()`: Filter interactively

## References

Swennen, M. (2018). Using Event Log Knowledge to Support Operational
Exellence Techniques (Doctoral dissertation). Hasselt University.

## See also

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

## Examples

``` r
eventdataR::patients %>%
  filter_precedence(antecedents = "Triage and Assessment",
            consequents = "Blood test",
            precedence_type = "directly_follows") %>%
  bupaR::traces()
#> # A tibble: 3 × 3
#>   trace                                    absolute_frequency relative_frequency
#>   <chr>                                                 <int>              <dbl>
#> 1 Registration,Triage and Assessment,Bloo…                234            0.987  
#> 2 Registration,Triage and Assessment,Bloo…                  2            0.00844
#> 3 Registration,Triage and Assessment,Bloo…                  1            0.00422

eventdataR::patients %>%
  filter_precedence(antecedents = "Triage and Assessment",
            consequents = c("Blood test", "X-Ray", "MRI SCAN"),
            precedence_type = "eventually_follows",
            filter_method = "one_of") %>%
  bupaR::traces()
#> # A tibble: 6 × 3
#>   trace                                    absolute_frequency relative_frequency
#>   <chr>                                                 <int>              <dbl>
#> 1 Registration,Triage and Assessment,X-Ra…                258            0.518  
#> 2 Registration,Triage and Assessment,Bloo…                234            0.470  
#> 3 Registration,Triage and Assessment,Bloo…                  2            0.00402
#> 4 Registration,Triage and Assessment,X-Ray                  2            0.00402
#> 5 Registration,Triage and Assessment,X-Ra…                  1            0.00201
#> 6 Registration,Triage and Assessment,Bloo…                  1            0.00201
```
