


trace_length_log <- function(eventlog) {

	trace_length_case(eventlog) -> raw

	raw %>%
		pull(trace_length) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	return(output)

}
