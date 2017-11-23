

filter_processing_time_percentile <- function(eventlog,
											  percentage,
											  reverse) {

	eventlog %>%
		processing_time("case") %>%
		arrange(processing_time) %>%
		slice(1:ceiling(n()*percentage)) %>%
		pull(1) -> case_selection

	filter_case(eventlog, case_selection, reverse)
}
