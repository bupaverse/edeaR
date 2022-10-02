
resource_frequency_resource_activity <- function(log) {
	absolute <- NULL
	relative_resource <- NULL
	relative_activity <- NULL

	log %>%
		distinct(!!resource_id_(log), !!activity_id_(log), !!activity_instance_id_(log)) %>%
		group_by(!!resource_id_(log), !!activity_id_(log)) %>%
		summarize(absolute = n()) %>%
		group_by(!!resource_id_(log)) %>%
		mutate(relative_resource = absolute/sum(absolute)) %>%
		group_by(!!activity_id_(log)) %>%
		mutate(relative_activity = absolute/sum(absolute)) %>%
		ungroup()

}
