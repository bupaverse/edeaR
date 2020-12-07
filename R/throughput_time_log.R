


throughput_time_log <- function(eventlog,
								units,
								work_schedule) {

	throughput_time_case(eventlog, units = units, work_schedule = work_schedule) -> raw


	raw %>%
		pull(throughput_time) %>%
		as.double(units = units) %>%
		summary_statistics() -> output


	attr(output, "raw") <- raw

	return(output)
}
