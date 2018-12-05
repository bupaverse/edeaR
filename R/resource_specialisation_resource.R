resource_specialisation_resource <- function(eventlog) {
	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_activities(eventlog))
	}
