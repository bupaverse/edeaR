
throughput_time_activity <- function(log, units,	work_schedule) {

	relative_frequency <- NULL

	throughput_time_activity_instance(log, units = units, work_schedule = work_schedule) -> raw
	log %>%
		as.data.frame() %>%
		distinct(!!sym(activity_id(log)), !!sym(activity_instance_id(log))) -> aid_a_link
	# Store time units, because dplyr transformations remove the attributes.
	time_units <- attr(raw, "units")


	merge(raw, aid_a_link, by = activity_instance_id(log)) %>%
		group_by(!!sym(activity_id(log))) %>%
		grouped_summary_statistics("throughput_time", relative_frequency = n()) %>%
		mutate(relative_frequency = relative_frequency/sum(relative_frequency)) %>%
		arrange(desc(relative_frequency)) %>%
		select(!!sym(activity_id(log)), relative_frequency, everything()) -> output

	attr(output, "raw") <- raw
	attr(output, "units") <- time_units
	return(output)
}
