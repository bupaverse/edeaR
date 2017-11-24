resource_frequency_activity <- function(eventlog) {
	nr_of_resources <- NULL
	eventlog %>%
		group_by(!!activity_id_(eventlog), !!resource_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
		summarize() %>%
		summarize(freq = n()) %>%
		grouped_summary_statistics("freq", nr_of_resources = n()) %>%
		select(!!activity_id_(eventlog), nr_of_resources, everything())
}
