
filter_trace_frequency_percentile <- function(eventlog,
								  percentage,
								  reverse){
	cum_sum <- NULL
	lag_cum_sum <- NULL

	eventlog %>%
		case_list() -> cases

	eventlog %>%
		trace_coverage("trace") %>%
		mutate(lag_cum_sum = lag(cum_sum, default = 0)) %>%
		filter(lag_cum_sum <= percentage) %>%
		merge(cases) %>%
		pull(!!case_id_(eventlog)) -> case_selection


	filter_case(eventlog, case_selection, reverse)
}
