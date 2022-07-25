resource_frequency_activity <- function(log) {
	nr_of_resources <- NULL
	log %>%
		distinct(!!activity_id_(log), !!resource_id_(log), !!activity_instance_id_(log)) %>%
		group_by(!!activity_id_(log), !!resource_id_(log)) %>%
		summarize(freq = n()) %>%
		group_by(!!activity_id_(log)) %>%
		grouped_summary_statistics("freq", nr_of_resources = n()) %>%
		select(!!activity_id_(log), nr_of_resources, everything())
}
