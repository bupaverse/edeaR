

filter_processing_time_percentile <- function(eventlog,
											  percentage,
											  reverse) {

	case_durations <- processing_time(eventlog = eventlog, "case") %>% arrange(processing_time)

	case_selection <- case_durations %>%
		slice(1:ceiling(nrow(case_durations)*percentage)) %>%
		pull(1)

	filter_case(eventlog, case_selection, reverse)
}
