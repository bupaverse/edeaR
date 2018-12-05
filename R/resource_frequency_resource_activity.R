
resource_frequency_resource_activity <- function(eventlog) {
	absolute <- NULL
	relative_resource <- NULL
	relative_activity <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		group_by(!!resource_id_(eventlog)) %>%
		mutate(relative_resource = absolute/sum(absolute)) %>%
		group_by(!!activity_id_(eventlog)) %>%
		mutate(relative_activity = absolute/sum(absolute))

}
