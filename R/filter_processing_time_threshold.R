
filter_processing_time_threshold <- function(log, lower_threshold, upper_threshold, reverse, units) {

	lower_threshold <- ifelse(is.na(lower_threshold), -Inf, lower_threshold)
	upper_threshold <- ifelse(is.na(upper_threshold), Inf, upper_threshold)

	case_selection <- log %>%
		processing_time("case", units = units) %>%
		filter(between(processing_time, lower_threshold, upper_threshold)) %>%
		pull(1)

	filter_case(log, cases = case_selection, reverse = reverse)
}
