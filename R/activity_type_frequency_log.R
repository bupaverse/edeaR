activity_type_frequency_log <- function(eventlog) {
	activity_type_frequency_case(eventlog) -> r

	summary_statistics(r$absolute) %>%
		return()
}
