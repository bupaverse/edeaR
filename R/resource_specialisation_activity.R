resource_specialisation_activity <- function(log) {

	absolute <- NULL
	relative <- NULL

	log %>%
		distinct(!!activity_id_(log), !!resource_id_(log)) %>%
		group_by(!!activity_id_(log)) %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(log))
}
