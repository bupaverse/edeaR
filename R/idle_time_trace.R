idle_time_trace <- function(log, units) {

	cases <- case_list(log)

	log %>%
		idle_time_case(units = units) %>%
		merge(cases) %>%
		group_by(trace) %>%
		grouped_summary_statistics("idle_time")
}
