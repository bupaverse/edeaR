


trace_length_log <- function(eventlog) {

	trace_length_case(eventlog) -> raw

	raw %>%
		pull(absolute) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	return(output)

}
