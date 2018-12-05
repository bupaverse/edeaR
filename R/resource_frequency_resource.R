
resource_frequency_resource <- function(eventlog) {
	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/sum(absolute))

}
