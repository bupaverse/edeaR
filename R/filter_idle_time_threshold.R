

filter_idle_time_threshold <- function(eventlog,
									   lower_threshold,
									   upper_threshold,
									   reverse,
									   units) {


	lower_threshold <- ifelse(is.na(lower_threshold), -Inf, lower_threshold)
	upper_threshold <- ifelse(is.na(upper_threshold), Inf, upper_threshold)

	eventlog %>%
		idle_time("case", units = units) %>%
		filter(between(as.double(idle_time, units = units), lower_threshold, upper_threshold)) %>%
		pull(1) -> case_selection

	filter_case(eventlog, case_selection, reverse)

}
