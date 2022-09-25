resource_specialisation_activity <- function(log) {

	absolute <- NULL
	relative <- NULL

	log %>%
		group_by(!!activity_id_(log), !!resource_id_(log)) %>%
		#summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(log))
}
