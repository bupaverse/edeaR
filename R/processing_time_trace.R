
processing_time_trace <- function(log, units, work_schedule) {

	relative_frequency <- NULL

	traces <- case_list(log)

	log %>%
		processing_time_case(units = units, work_schedule = work_schedule)  -> raw

	time_units <- attr(raw, "units")

	raw %>%
		merge(traces) %>%
		group_by(trace) %>%
		grouped_summary_statistics("processing_time", relative_frequency = n()) %>%
		mutate(relative_frequency = relative_frequency/sum(relative_frequency)) %>%
		select(trace, relative_frequency, everything()) %>%
		arrange(desc(relative_frequency)) -> output

	attr(output, "raw") <- raw
	attr(output, "units") <- time_units
	return(output)
}
