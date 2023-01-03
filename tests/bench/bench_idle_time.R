data <- eventdataR::patients

bench::mark(
    log = idle_time(data, level = "log", units = "mins"),
    trace = idle_time(data, level = "trace", units = "mins"),
    case = idle_time(data, level = "case", units = "mins"),
    resource = idle_time(data, level = "resource", units = "mins"),
    iterations = 5,
    check = FALSE
)

# Old implementation
# A tibble: 1 x 8
#min        q1            median        mean          q3         max           st_dev iqr
#<drtn>     <drtn>        <drtn>        <drtn>        <drtn>     <drtn>         <dbl> <drtn>
#1 899.1 mins 4613.517 mins 7164.167 mins 7948.915 mins 10690 mins 31503.38 mins  4614. 6076.479 mins
# A tibble: 4 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time           gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>         <list>
#1 log           10.4s    10.6s    0.0922    43.7MB     1.60     5    87      54.2s <NULL> <Rprofmem> <bench_tm [5]> <tibble>
#2 trace         10.7s    10.8s    0.0917   45.09MB     1.61     5    88      54.5s <NULL> <Rprofmem> <bench_tm [5]> <tibble>
#3 case          10.7s    10.9s    0.0921   43.62MB     1.62     5    88      54.3s <NULL> <Rprofmem> <bench_tm [5]> <tibble>
#4 resource     47.8ms   52.4ms   19.2       2.78MB     0        5     0    260.4ms <NULL> <Rprofmem> <bench_tm [5]> <tibble>

# New data.table implementation
# A tibble: 4 x 13
#expression      min   median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result memory     time           gc
#<bch:expr> <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list> <list>     <list>         <list>
#1 log          30.1ms   32.9ms      29.9    1.35MB     7.48     4     1    133.6ms <NULL> <Rprofmem> <bench_tm [5]> <tibble>
#2 trace        79.2ms   83.3ms      11.8    2.69MB     0        5     0    425.2ms <NULL> <Rprofmem> <bench_tm [5]> <tibble>
#3 case         28.5ms   29.2ms      34.4    1.32MB     8.60     4     1    116.3ms <NULL> <Rprofmem> <bench_tm [5]> <tibble>
#4 resource     15.2ms   15.4ms      65.1    1.25MB     0        5     0     76.8ms <NULL> <Rprofmem> <bench_tm [5]> <tibble>
