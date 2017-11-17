

filter_processing_time_threshold <- function(eventlog,
											 lower_threshold,
											 upper_threshold,
											 reverse,
											 units)
{
	lower_threshold <- ifelse(is.na(lower_threshold), -Inf, lower_threshold)
	upper_threshold <- ifelse(is.na(upper_threshold), Inf, upper_threshold)

	case_selection <- eventlog %>%
		processing_time("case", units = units) %>%
		filter(between(processing_time, lower_threshold, upper_threshold)) %>%
		pull(1)

	filter_case(eventlog, case_selection, reverse)

}
