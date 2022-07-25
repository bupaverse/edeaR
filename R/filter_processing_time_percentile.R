
filter_processing_time_percentile <- function(log, percentage, reverse) {

	log %>%
		processing_time("case") %>%
		arrange(processing_time) %>%
		slice(1:ceiling(n()*percentage)) %>%
		pull(1) -> case_selection

	filter_case(log, cases = case_selection, reverse = reverse)
}
