
processing_time_activity <- function(log, units, work_schedule) {

	relative_frequency <- NULL

	log %>%
		processing_time_activity_instance(units = units, work_schedule = work_schedule) -> raw

	time_units <- attr(raw, "units")

	log %>%
		distinct(!!activity_instance_id_(log), !!activity_id_(log)) -> dict

	dict %>%
		full_join(raw, by = activity_instance_id(log)) %>%
		as_tibble()-> raw

	raw %>%
		group_by(!!activity_id_(log)) %>%
		grouped_summary_statistics("processing_time", relative_frequency = n()) %>%
		mutate(relative_frequency = relative_frequency/sum(relative_frequency)) %>%
		arrange(desc(relative_frequency)) -> output

	attr(output, "raw") <- raw
	attr(output, "units") <- time_units
	return(output)
}
