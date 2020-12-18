
processing_time_activity <- function(eventlog,
									 units,
									 work_schedule) {

	relative_frequency <- NULL


	eventlog %>%
		processing_time_activity_instance(units = units, work_schedule = work_schedule) -> raw

	eventlog %>%
		distinct(!!activity_instance_id_(eventlog), !!activity_id_(eventlog)) -> dict

	dict %>%
		full_join(raw, by = activity_instance_id(eventlog)) %>%
		as_tibble()-> raw

	raw %>%
		group_by(!!activity_id_(eventlog)) %>%
		grouped_summary_statistics("processing_time", relative_frequency = n()) %>%
		mutate(relative_frequency = relative_frequency/sum(relative_frequency)) %>%
		arrange(desc(relative_frequency)) -> output

	attr(output, "raw") <- raw

	return(output)

}
