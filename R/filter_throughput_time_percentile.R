

filter_throughput_time_percentile <- function(eventlog,
											  percentage,
											  reverse) {

	case_selection <- eventlog %>%
		throughput_time("case") %>%
		arrange(throughput_time) %>%
		slice(1:ceiling(n()*percentage)) %>%
		pull(1)

	filter_case(eventlog, case_selection, reverse)
}
