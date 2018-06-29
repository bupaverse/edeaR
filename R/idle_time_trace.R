idle_time_trace <- function(eventlog, units) {

	cases <- case_list(eventlog)

	eventlog %>%
		idle_time_case(units = units) %>%
		merge(cases) %>%
		group_by(trace) %>%
		grouped_summary_statistics("idle_time")
}
