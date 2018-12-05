resource_specialisation_activity <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by(!!activity_id_(eventlog), !!resource_id_(eventlog)) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(eventlog))
}
