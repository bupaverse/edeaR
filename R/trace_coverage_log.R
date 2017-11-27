
trace_coverage_log <- function(eventlog,
						   threshold = NULL) {
	relative <- NULL

	trace_coverage_trace(eventlog) -> raw
	raw %>%
		pull(relative) %>%
		summary_statistics() -> output
	attr(output, "raw") <- raw
	return(output)
}
