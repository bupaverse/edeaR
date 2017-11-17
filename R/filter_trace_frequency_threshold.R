


filter_trace_frequency_threshold <- function(eventlog,
											  lower_threshold,
											  upper_threshold,
											  reverse){

	if(is.null(lower_threshold) & is.null(upper_threshold)){
		stop("Upper threshold or lower threshold must be defined")
	}
	if(is.na(lower_threshold))
		lower_threshold <- -Inf
	if(is.na(upper_threshold))
		upper_threshold <- Inf


	eventlog %>%
		trace_coverage("case") %>%
		filter(absolute_trace_coverage >= lower_threshold,
			   absolute_trace_coverage <= upper_threshold) %>%
		pull(1) -> case_selection

	filter_case(eventlog, case_selection, reverse)

}
