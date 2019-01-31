
filter_trace_length_threshold <- function(eventlog,
									  lower_threshold,
									  upper_threshold,
									  reverse)
{
	absolute <- NULL

	if(is.na(lower_threshold))
		lower_threshold <- -Inf
	if(is.na(upper_threshold))
		upper_threshold <- Inf


	eventlog %>%
		trace_length("case") %>%
		filter(between(absolute, lower_threshold, upper_threshold)) %>%
		pull(1) -> case_selection

	filter_case(eventlog, case_selection, reverse)
}
