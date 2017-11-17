
filter_trace_frequency_percentile <- function(eventlog,
								  percentage,
								  reverse){

	eventlog %>%
		cases_light() -> cases

	eventlog %>%
		trace_coverage("trace") %>%
		mutate(lag_cum_sum = lag(cum_sum, default = 0)) %>%
		filter(lag_cum_sum <= percentage) %>%
		merge(cases) %>%
		pull(!!as.symbol(case_id(eventlog))) -> case_selection


	filter_case(eventlog, case_selection, reverse)
}
