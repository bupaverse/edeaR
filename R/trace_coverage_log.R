
trace_coverage_log <- function(log) {

	relative <- NULL

	trace_coverage_trace(log) -> raw
	raw %>%
		pull(relative) %>%
		summary_statistics() -> output
	attr(output, "raw") <- raw
	return(output)
}
