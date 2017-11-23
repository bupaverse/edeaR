

filter_throughput_time_threshold <- function(eventlog,
									  lower_threshold,
									  upper_threshold,
									  reverse,
									  units) {


	lower_threshold <- ifelse(is.na(lower_threshold), -Inf, lower_threshold)
	upper_threshold <- ifelse(is.na(upper_threshold), Inf, upper_threshold)

	eventlog %>%
		throughput_time("case", units = units) %>%
		filter(between(throughput_time, lower_threshold, upper_threshold)) %>%
		pull(1) -> case_selection

	filter_case(eventlog, case_selection, reverse)

}
