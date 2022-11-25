

filter_idle_time_percentile <- function(log,
										percentage,
										reverse) {

	case_selection <- log %>%
		idle_time(level = "case") %>%
		arrange(idle_time) %>%
		slice(1:ceiling(n()*percentage)) %>%
		pull(1)

	filter_case(log, case_selection, reverse)
}
