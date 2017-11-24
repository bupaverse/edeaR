



processing_time_activity_instance <- function(eventlog,
											  units) {
	s <- NULL
	e <- NULL

	eventlog %>%
		group_by(!!activity_instance_id_(eventlog),
				 !!case_id_(eventlog),
				 !!activity_id_(eventlog),
				 !!resource_id_(eventlog)) %>%
		summarize(s = min(!!timestamp_(eventlog)), e = max(!!timestamp_(eventlog))) %>%
		mutate(processing_time = as.double(e - s, units = units)) %>%
		ungroup()

}
