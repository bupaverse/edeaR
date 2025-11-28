# Filters

``` r
library(bupaR)
library(edeaR)
library(eventdataR)
```

The filters for event data subsetting can mostly be divided into two
type: event filters and case filters. Event filters will subset *parts*
of cases based on criteria applied on the events (e.g. the resource
which performed it), while case filters will subset complete cases,
based on criteria applied on the cases (e.g. the trace length).

Each filter has a *reverse* argument, which allows to reverse the filter
very easily. Furthermore, each filter has an interface-alternative,
which can be called by adding a *i* before the function name.

## Event filters

### Filter activities

The filter activity function can be used to filter activities by name.
It has three arguments

- the event log
- a vector of activities
- the reverse argument (FALSE or TRUE)

``` r
patients %>%
    filter_activity(c("X-Ray", "Blood test")) %>%
    summary
```

    ## Number of events:  996
    ## Number of cases:  498
    ## Number of traces:  2
    ## Number of distinct activities:  2
    ## Average trace length:  2
    ## 
    ## Start eventlog:  2017-01-05 08:59:04
    ## End eventlog:  2018-05-05 01:34:30

    ##                   handling     patient          employee handling_id       
    ##  Blood test           :474   Length:996         r1:  0   Length:996        
    ##  Check-out            :  0   Class :character   r2:  0   Class :character  
    ##  Discuss Results      :  0   Mode  :character   r3:474   Mode  :character  
    ##  MRI SCAN             :  0                      r4:  0                     
    ##  Registration         :  0                      r5:522                     
    ##  Triage and Assessment:  0                      r6:  0                     
    ##  X-Ray                :522                      r7:  0                     
    ##  registration_type      time                         .order     
    ##  complete:498      Min.   :2017-01-05 08:59:04   Min.   :  1.0  
    ##  start   :498      1st Qu.:2017-05-06 12:31:43   1st Qu.:249.8  
    ##                    Median :2017-09-08 00:10:11   Median :498.5  
    ##                    Mean   :2017-09-03 07:11:55   Mean   :498.5  
    ##                    3rd Qu.:2017-12-23 02:06:20   3rd Qu.:747.2  
    ##                    Max.   :2018-05-05 01:34:30   Max.   :996.0  
    ## 

As one can see, there are only 2 distinct activities left in the event
log.

### Filter on activity frequency

It is also possible to filter on activity frequency. This filter uses a
percentile cut off, and will look at those activities which are most
frequent until the required percentage of events has been reached. Thus,
a percentile cut off of 80% will look at the activities needed to
represent 80% of the events. In the example below, the **least**
frequent activities covering 50% of the event log are selected, since
the reverse argument is true.

``` r
patients %>%
    filter_activity_frequency(percentage = 0.5, reverse = T) %>%
    activity_frequency("activity")
```

    ## # A tibble: 4 × 3
    ##   handling   absolute relative
    ##   <fct>         <int>    <dbl>
    ## 1 Check-out       492    0.401
    ## 2 X-Ray           261    0.213
    ## 3 Blood test      237    0.193
    ## 4 MRI SCAN        236    0.192

### Filter on attributes

The filter_attributes function is a very generic function an can be
supplied with conditions on the data set, in the same way as the
[`dplyr::filter`](https://dplyr.tidyverse.org/reference/filter.html)
function. As such, it allows you to filter on event or case attributes.
Multiple conditions can be listed, separated by a comma. In that case,
the comma will be treated as “and”. You can use the \|-symbol to state
“OR”. Since the patients dataset does not have many additional
attributes, the example below uses the resource and activity. This
filter is thus the same as the combination of filter_activity and
filter_resource, in case both conditions were required. However, it has
the advantange of stating both conditions as OR.

``` r
patients %>% 
    filter(employee == "r1" | handling == "X-Ray") 
```

    ## # Log of 1522 events consisting of:
    ## 2 traces 
    ## 500 cases 
    ## 761 instances of 2 activities 
    ## 2 resources 
    ## Events occurred from 2017-01-02 11:41:53 until 2018-05-05 01:34:30 
    ##  
    ## # Variables were mapped as follows:
    ## Case identifier:        patient 
    ## Activity identifier:     handling 
    ## Resource identifier:     employee 
    ## Activity instance identifier:    handling_id 
    ## Timestamp:           time 
    ## Lifecycle transition:        registration_type 
    ## 
    ## # A tibble: 1,522 × 7
    ##    handling   patient employee handling_id registration_type time               
    ##    <fct>      <chr>   <fct>    <chr>       <fct>             <dttm>             
    ##  1 Registrat… 1       r1       1           start             2017-01-02 11:41:53
    ##  2 Registrat… 2       r1       2           start             2017-01-02 11:41:53
    ##  3 Registrat… 3       r1       3           start             2017-01-04 01:34:05
    ##  4 Registrat… 4       r1       4           start             2017-01-04 01:34:04
    ##  5 Registrat… 5       r1       5           start             2017-01-04 16:07:47
    ##  6 Registrat… 6       r1       6           start             2017-01-04 16:07:47
    ##  7 Registrat… 7       r1       7           start             2017-01-05 04:56:11
    ##  8 Registrat… 8       r1       8           start             2017-01-05 04:56:11
    ##  9 Registrat… 9       r1       9           start             2017-01-06 05:58:54
    ## 10 Registrat… 10      r1       10          start             2017-01-06 05:58:54
    ## # ℹ 1,512 more rows
    ## # ℹ 1 more variable: .order <int>

### Filter resources

Similar to the activity filter, the resource filter can be used to
filter events by listing on or more resources.

``` r
patients %>%
    filter_resource(c("r1","r4")) %>%
    resource_frequency("resource")
```

    ## # A tibble: 2 × 3
    ##   employee absolute relative
    ##   <fct>       <int>    <dbl>
    ## 1 r1            500    0.679
    ## 2 r4            236    0.321

### Trim cases

The trim filter is a special event filter, as it also take into account
the notion of cases. In fact, it *trim* cases such that they start with
a certain activities until they end with a certain activity. It requires
two list: one for possible start activities and one for end activities.
The cases will be trimmed from the first appearance of a start activity
till the last appearance of an end activity. When reversed, these
*slices* of the event log will be removed instead of preserved.

``` r
patients %>%
    filter_trim(start_activities = "Registration", end_activities =  c("MRI SCAN","X-Ray")) %>%
    traces()
```

    ## # A tibble: 2 × 3
    ##   trace                                    absolute_frequency relative_frequency
    ##   <chr>                                                 <int>              <dbl>
    ## 1 Registration,Triage and Assessment,X-Ray                261              0.525
    ## 2 Registration,Triage and Assessment,Bloo…                236              0.475

## Case filters

#### Filter activity presence

This functions allows to filter cases that contain certain activities.
It requires as input a vector containing one or more activity labels and
it has a `method` argument. The latter can have the values *all*, *none*
or *one_of*. When set to *all*, it means that all the specified activity
labels must be present for a case to be selected, *none* means that they
are not allowed to be present, and *one_of* means that at least one of
them must be present.

#### Filter case

The case filter allows to subset a set of case identifiers. As arguments
it only requires a vector of case id’s. The selection can also be
negated using `reverse = T`.

#### Filter end points

The `filter_endpoints` method filters cases based on the first and last
activity label. It can be used in two ways: by specifying vectors with
allowed start activities and/or allowed end activities, or by specifying
a percentile. In the latter case, the percentile value will be used as a
cut off. For example, when set to 0.9, it will select the most common
endpoint pairs which together cover at least 90% of the cases, and
filter the event log accordingly. This filter can also be reversed.

#### Filter precedence

In order to extract a subset of an event log which conforms with a set
of precedence rules, one can use the `filter_precedence` method. There
are two types of precendence relations which can be tested: activities
that should *directly follow* each other, or activities that should
*eventually follow* each other. The type can be set with the
*precedence_type* argument. Further, the filter requires a vector of one
or more antecedents (containing activity labels), and one or more
consequents. Finally, also a *filter_method* argument can be set. This
argument is relevant when there is more than one antecedent or
consequent. In such a case, you can specify that all possible precedence
combinations must be present (*all*), or at least one of them
(\_one_of).

#### Filter processing time, throughput time and trace length

There are three different filters which take into account the *length*
of a case:

- processing time: which is the sum of the duration of the activity
  instances contained in the case.
- throughput time: which is the time between the first event and the
  last event of the case.
- trace length: which is the number of activity instances contained in
  the case.

Each of these filters can work in two ways, similar to the endpoints
filter: either by using an interval or by using a percentile cut off.
The percentile cut off will always start with the shortest cases first
and stop including cases when the specified percentile is reached. The
processing and throughput time filters also have a *units* attribute to
specify the time unit used when defining an interval. All the methods
can be reversed by setting `reverse = T`.

#### Filter time period

Cases can also be filtered by supplying a time window to the method
`filter_time_period`. There are four different filter methods, of which
one can be used as argument:

- contained: retains all cases which are completely contained in the
  time period.
- start: retains the cases which started in the time period, regardless
  of their end point.
- complete: retains the cases which were completed in the time period,
  regardless of their starting point.
- intersecting: retains the cases which have at least one event within
  the time period.

The selection can also be reversed. Note that there is a 5 filter
method, *trim*, but this is actually an event filter and will thus be
discussed in the next section.

#### Filter trace frequency

The last case filter can be used to filter cases based on the frequency
of the corresponding trace. A trace is a sequence of activity labels,
and will be discussed in more detail in Section . There are again two
ways to select cases based on trace frequency, by interval or by
percentile cut off. The percentile cut off will start with the most
frequent traces. This filter also contains the reverse argument.
