
filter_trace_length_percentile <- function(eventlog,
								percentage,
								reverse)
{

	eventlog %>%
		trace_length("case") %>%
		slice(1:ceiling(n()*percentage)) %>%
		pull(1) -> case_selection

	filter_case(eventlog, case_selection, reverse)
}
