# Performance with work schedules

When computing the throughput or processsing time, you can take into
account a predefined working schedule. In this way, you can controle for
working hours and holidays.

## Creating a working schedule

A default work schedule can be created using the function
`create_working_schedule`

``` r
library(edeaR)
create_work_schedule()
```

    ## Week schedule
    ## # A tibble: 7 × 3
    ##     day start_time end_time
    ##   <int> <time>     <time>  
    ## 1     1 09:00      17:00   
    ## 2     2 09:00      17:00   
    ## 3     3 09:00      17:00   
    ## 4     4 09:00      17:00   
    ## 5     5 09:00      17:00   
    ## 6     6    NA         NA   
    ## 7     7    NA         NA   
    ## 
    ## Fixed holidays
    ## # A tibble: 2 × 3
    ##   month   day name          
    ##   <dbl> <dbl> <chr>         
    ## 1     1     1 New Year's Day
    ## 2    12    25 Christmas     
    ## 
    ## Floating holidays
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: date <date>, name <chr>
    ## 
    ## Holiday periods
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: from <date>, to <date>

It will generate a weekly working hours schedule, defaulting to 9 to 5
workdays. You can change the day schedule using the arguments
`start_time` and/or `end_time`. The following code will generate a work
schedule with workdays from 8.30am to 4pm.

``` r
create_work_schedule(start_time = "08:30:00", end_time = "16:00:00")
```

    ## Week schedule
    ## # A tibble: 7 × 3
    ##     day start_time end_time
    ##   <int> <time>     <time>  
    ## 1     1 08:30      16:00   
    ## 2     2 08:30      16:00   
    ## 3     3 08:30      16:00   
    ## 4     4 08:30      16:00   
    ## 5     5 08:30      16:00   
    ## 6     6    NA         NA   
    ## 7     7    NA         NA   
    ## 
    ## Fixed holidays
    ## # A tibble: 2 × 3
    ##   month   day name          
    ##   <dbl> <dbl> <chr>         
    ## 1     1     1 New Year's Day
    ## 2    12    25 Christmas     
    ## 
    ## Floating holidays
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: date <date>, name <chr>
    ## 
    ## Holiday periods
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: from <date>, to <date>

Once a work schedule is created, you can also change individal days. For
example, suppose that on Friday the office closes as 1pm, this can be
changed as follows. Note that the days are numbered starting on monday.

``` r
create_work_schedule(start_time = "08:30:00", end_time = "16:00:00") %>%
    change_day(5, start_time = "08:30:00", end_time = "13:00:00")
```

    ## Week schedule
    ## # A tibble: 7 × 3
    ##     day start_time end_time
    ##   <int> <time>     <time>  
    ## 1     1 08:30      16:00   
    ## 2     2 08:30      16:00   
    ## 3     3 08:30      16:00   
    ## 4     4 08:30      16:00   
    ## 5     5 08:30      13:00   
    ## 6     6    NA         NA   
    ## 7     7    NA         NA   
    ## 
    ## Fixed holidays
    ## # A tibble: 2 × 3
    ##   month   day name          
    ##   <dbl> <dbl> <chr>         
    ## 1     1     1 New Year's Day
    ## 2    12    25 Christmas     
    ## 
    ## Floating holidays
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: date <date>, name <chr>
    ## 
    ## Holiday periods
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: from <date>, to <date>

Next to the working times, the work schedule also contains information
on holidays. Three different types should be distinguished:

- **fixed holidays**: these are holidays that take place on the same
  date each year, such as Christmas, New Year’s Day, etc.
- **floating holidays**: there are holidays that take place on a
  different date each year, such as Easter, or Thanksgiving.
- **holiday periods**: these are additional periods that you can define,
  for instance, if your organisation is closed during summer holidays.

All holidays are excluded in the calculation of throughput and
processing time. By default, the `create_working_schedule` function
creates two fixed holidays: Christmas and New Year’s day. Let’s further
add some more holidays.

Another fixed holiday that we can typically add, are national holidays.
In Belgium, this would be the 21st of July.

``` r
create_work_schedule(start_time = "08:30:00", end_time = "16:00:00") %>%
    change_day(5, start_time = "08:30:00", end_time = "13:00:00") %>%
    add_fixed_holiday("Belgian National Holiday", 07, 21)
```

    ## Week schedule
    ## # A tibble: 7 × 3
    ##     day start_time end_time
    ##   <int> <time>     <time>  
    ## 1     1 08:30      16:00   
    ## 2     2 08:30      16:00   
    ## 3     3 08:30      16:00   
    ## 4     4 08:30      16:00   
    ## 5     5 08:30      13:00   
    ## 6     6    NA         NA   
    ## 7     7    NA         NA   
    ## 
    ## Fixed holidays
    ## # A tibble: 3 × 3
    ##   month   day name                    
    ##   <dbl> <dbl> <chr>                   
    ## 1     1     1 New Year's Day          
    ## 2    12    25 Christmas               
    ## 3     7    21 Belgian National Holiday
    ## 
    ## Floating holidays
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: date <date>, name <chr>
    ## 
    ## Holiday periods
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: from <date>, to <date>

A typical floating holiday is Easter. However, as Easter falls on a
Sunday, it is already not taking into account. Nonetheless, let us add
Easter Monday to the schedule.

For floating holidays, it is important to add all dates relevant for
your data, that is, for all the years on which you have data.

Suppose we will be using the `patients` dataset. This stretched from
2017 to 2018, so we need to add Easter Monday of both years, which are
2017-04-17 and 2018-04-02.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
create_work_schedule(start_time = "08:30:00", end_time = "16:00:00") %>%
    change_day(5, start_time = "08:30:00", end_time = "13:00:00") %>%
    add_fixed_holiday("Belgian National Holiday", 07, 21) %>%
    add_floating_holiday("Easter Monday", ymd(c(20170417, 20180402)))
```

    ## Week schedule
    ## # A tibble: 7 × 3
    ##     day start_time end_time
    ##   <int> <time>     <time>  
    ## 1     1 08:30      16:00   
    ## 2     2 08:30      16:00   
    ## 3     3 08:30      16:00   
    ## 4     4 08:30      16:00   
    ## 5     5 08:30      13:00   
    ## 6     6    NA         NA   
    ## 7     7    NA         NA   
    ## 
    ## Fixed holidays
    ## # A tibble: 3 × 3
    ##   month   day name                    
    ##   <dbl> <dbl> <chr>                   
    ## 1     1     1 New Year's Day          
    ## 2    12    25 Christmas               
    ## 3     7    21 Belgian National Holiday
    ## 
    ## Floating holidays
    ## # A tibble: 2 × 2
    ##   date       name         
    ##   <date>     <chr>        
    ## 1 2017-04-17 Easter Monday
    ## 2 2018-04-02 Easter Monday
    ## 
    ## Holiday periods
    ## # A tibble: 0 × 2
    ## # ℹ 2 variables: from <date>, to <date>

Finally, let us assume that we don’t work in the period between
Christmas and New year in 2017. We can add a holiday period from
December 26th till December 31st.

``` r
library(lubridate)
create_work_schedule(start_time = "08:30:00", end_time = "16:00:00") %>%
    change_day(5, start_time = "08:30:00", end_time = "13:00:00") %>%
    add_fixed_holiday("Belgian National Holiday", month =  07, day = 21) %>%
    add_floating_holiday("Easter Monday", dates = ymd(c(20170417, 20180402))) %>%
    add_holiday_periods(from = ymd(20171226), to = ymd(20171231))
```

    ## Week schedule
    ## # A tibble: 7 × 3
    ##     day start_time end_time
    ##   <int> <time>     <time>  
    ## 1     1 08:30      16:00   
    ## 2     2 08:30      16:00   
    ## 3     3 08:30      16:00   
    ## 4     4 08:30      16:00   
    ## 5     5 08:30      13:00   
    ## 6     6    NA         NA   
    ## 7     7    NA         NA   
    ## 
    ## Fixed holidays
    ## # A tibble: 3 × 3
    ##   month   day name                    
    ##   <dbl> <dbl> <chr>                   
    ## 1     1     1 New Year's Day          
    ## 2    12    25 Christmas               
    ## 3     7    21 Belgian National Holiday
    ## 
    ## Floating holidays
    ## # A tibble: 2 × 2
    ##   date       name         
    ##   <date>     <chr>        
    ## 1 2017-04-17 Easter Monday
    ## 2 2018-04-02 Easter Monday
    ## 
    ## Holiday periods
    ## # A tibble: 1 × 2
    ##   from       to        
    ##   <date>     <date>    
    ## 1 2017-12-26 2017-12-31

Note that it doesn’t make much sense to use a working schedule for the
patients data, as work in a healthcare environment doesn’t obey working
hours. But for the sake of illustration, and because the patients data
includes start and complete events, let’s continue.

Let’s save our work schedule as `ws`.

``` r
ws <- create_work_schedule(start_time = "08:30:00", end_time = "16:00:00") %>%
    change_day(5, start_time = "08:30:00", end_time = "13:00:00") %>%
    add_fixed_holiday("Belgian National Holiday", month =  07, day = 21) %>%
    add_floating_holiday("Easter Monday", dates = ymd(c(20170417, 20180402))) %>%
    add_holiday_periods(from = ymd(20171226), to = ymd(20171231))
```

## Calculating performance

We can now plug the working schedule in any processing or throughput
time calculation.

For example, throughput time would normally be computed as follows.

``` r
library(eventdataR)
patients %>% throughput_time()
```

    ## # A tibble: 1 × 8
    ##   min           q1            median        mean        q3    max   st_dev iqr  
    ##   <drtn>        <drtn>        <drtn>        <drtn>      <drt> <drt>  <dbl> <drt>
    ## 1 1.496088 days 4.313924 days 6.085509 days 6.676308 d… 8.58… 23.1…   3.22 4.27…

In order to take into account the working schedule

``` r
patients %>% throughput_time(work_schedule = ws)
```

    ## # A tibble: 1 × 8
    ##     min    q1 median    mean     q3    max st_dev   iqr
    ##   <dbl> <dbl>  <dbl>   <dbl>  <dbl>  <dbl>  <dbl> <dbl>
    ## 1     0 70200 108000 115625. 151200 426600 59688. 81000

As another example, consider the processing time by activity.

Without taking into account the working hours:

``` r
patients %>%
    processing_time(level = "activity")
```

    ## # A tibble: 7 × 11
    ##   handling              min    q1    mean  median q3    max   st_dev   iqr total
    ##   <fct>                 <drtn> <drt> <drt> <drtn> <drt> <drt>  <dbl> <dbl> <drt>
    ## 1 Registration           49.7… 124.… 165.… 162.8… 204.…  338…   57.2  79.9  826…
    ## 2 Triage and Assessment 352.1… 681.… 786.… 800.4… 901.… 1128…  166.  221.  3931…
    ## 3 Discuss Results        80.0… 138.… 166.… 166.3… 193.…  272…   37.7  54.4  824…
    ## 4 Check-out              40.0…  96.… 123.… 124.3… 148.…  233…   37.2  51.6  608…
    ## 5 X-Ray                 137.6… 233.… 290.… 287.5… 338.…  490…   76.9 106.   758…
    ## 6 Blood test            185.3… 285.… 332.… 328.1… 376.…  488…   63.6  90.7  787…
    ## 7 MRI SCAN              149.3… 216.… 248.… 245.4… 281.…  355…   44.1  65.4  587…
    ## # ℹ 1 more variable: relative_frequency <dbl>

With the working hours:

``` r
patients %>%
    processing_time(level = "activity", work_schedule = ws)
```

    ## Warning: Work schedule currently not supporting for processing time

    ## # A tibble: 7 × 11
    ##   handling              min    q1    mean  median q3    max   st_dev   iqr total
    ##   <fct>                 <drtn> <drt> <drt> <drtn> <drt> <drt>  <dbl> <dbl> <drt>
    ## 1 Registration           49.7… 124.… 165.… 162.8… 204.…  338…   57.2  79.9  826…
    ## 2 Triage and Assessment 352.1… 681.… 786.… 800.4… 901.… 1128…  166.  221.  3931…
    ## 3 Discuss Results        80.0… 138.… 166.… 166.3… 193.…  272…   37.7  54.4  824…
    ## 4 Check-out              40.0…  96.… 123.… 124.3… 148.…  233…   37.2  51.6  608…
    ## 5 X-Ray                 137.6… 233.… 290.… 287.5… 338.…  490…   76.9 106.   758…
    ## 6 Blood test            185.3… 285.… 332.… 328.1… 376.…  488…   63.6  90.7  787…
    ## 7 MRI SCAN              149.3… 216.… 248.… 245.4… 281.…  355…   44.1  65.4  587…
    ## # ℹ 1 more variable: relative_frequency <dbl>

## Caution

Some caution is required when using the work schedules in your
calculations. If a case falls completely in a holiday period, or during
a weekend, it will receive a throughput time of zero. The same goes for
activities that take place outside of working hours, when computing
processing time.

If an activity starts at 7am and is completed at 10am. but your working
schedule has 9-to-5 workdays, the activity will have a processing time
of only 1 hour. If it was completed anytime before 9am, it will be zero!

As such, using a working schedule will overestimate your performance if
a lot of activities doesn’t adhere to the working schedule. At this
moment, the performance functions will **not** notify you if this is the
case. Make sure to only use a working schedule if the recorded events
fall inside the working schedule most of the time.
