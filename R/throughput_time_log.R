


throughput_time_log <- function(eventlog,
								 units) {

	throughput_time_case(eventlog, units = units) -> raw


	raw %>%
		pull(throughput_time) %>%
		as.double(units = units) %>%
		summary_statistics() -> output


	attr(output, "raw") <- raw

	return(output)
}
