
throughput_time_trace <- function(log, units, work_schedule) {

	relative_frequency <- NULL

	t <- throughput_time_case(log, units = units, work_schedule = work_schedule)
	c <- case_list(log)

	# Store time units, because dplyr transformations remove the attributes.
	time_units <- attr(t, "units")

	merge(t, c, by = case_id(log)) %>%
		group_by(trace) %>%
		grouped_summary_statistics("throughput_time", relative_frequency = n()) %>%
		mutate(relative_frequency = relative_frequency/sum(relative_frequency)) %>%
		arrange(desc(relative_frequency)) %>%
		select(trace, relative_frequency, everything()) -> output

	attr(output, "units") <- time_units
	return(output)
}
