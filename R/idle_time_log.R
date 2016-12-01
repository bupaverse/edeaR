idle_time_log <- function(eventlog, units) {

	idle_times <- idle_time_case(eventlog, units)

	summary_statistics(idle_times$idle_time) %>%
		return()
}
