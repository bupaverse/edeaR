

throughput_time_trace <- function(eventlog, units) {
	relative_trace_frequency <- NULL

	t <- throughput_time_case(eventlog, units = units)
	c <- case_list(eventlog)

	merge(t, c, by = case_id(eventlog)) %>%
		group_by(trace) %>%
		grouped_summary_statistics("throughput_time", relative_trace_frequency = n()) %>%
		mutate(relative_trace_frequency = relative_trace_frequency/sum(relative_trace_frequency)) %>%
		arrange(desc(relative_trace_frequency)) %>%
		select(trace, relative_trace_frequency, everything())

}
