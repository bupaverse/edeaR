
processing_time_case <- function(log, units, work_schedule) {

	log %>%
		distinct(!!case_id_(log), !!activity_instance_id_(log)) -> dict

	log %>%
		processing_time_activity_instance(units = units,
										  work_schedule = work_schedule) -> raw

	time_units <- attr(raw, "units")

	dict <- dict %>%
		full_join(raw, by = activity_instance_id(log)) %>%
		group_by(!!case_id_(log)) %>%
		summarize(processing_time = sum(processing_time)) %>%
		select(!!case_id_(log), processing_time)

	attr(dict, "units") <- time_units
	return(dict)
}
