
trace_length_log <- function(log) {
	absolute <- NULL
	trace_length_case(log) -> raw

	raw %>%
		pull(absolute) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	return(output)

}
