
trace_coverage_log <- function(eventlog,
						   threshold = NULL) {


	trace_coverage_trace(eventlog) -> t
	t %>%
		pull(relative) %>%
		summary_statistics() -> output
	attr(output, "raw") <- t
	return(output)
}
