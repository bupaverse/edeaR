

filter_idle_time_percentile <- function(eventlog,
											  percentage,
											  reverse) {

	case_selection <- eventlog %>%
		idle_time("case") %>%
		arrange(idle_time) %>%
		slice(1:ceiling(n()*percentage)) %>%
		pull(1)

	filter_case(eventlog, case_selection, reverse)
}
