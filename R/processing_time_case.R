

processing_time_case <- function(eventlog,
								 units,
								 work_schedule) {

	eventlog %>%
		distinct(!!case_id_(eventlog), !!activity_instance_id_(eventlog)) -> dict

	eventlog %>%
		processing_time_activity_instance(units = units,
										  work_schedule = work_schedule) -> raw


	dict %>%
		full_join(raw, by = activity_instance_id(eventlog)) %>%
		group_by(!!case_id_(eventlog)) %>%
		summarize(processing_time = sum(processing_time)) %>%
		select(!!case_id_(eventlog), processing_time)

}
