

processing_time_case <- function(eventlog,
								 units,
								 work_schedule) {

	eventlog %>%
		processing_time_activity_instance(units = units,
										  work_schedule = work_schedule) %>%
		group_by(!!case_id_(eventlog)) %>%
		summarize(processing_time = sum(processing_time)) %>%
		select(!!case_id_(eventlog), processing_time)

}
