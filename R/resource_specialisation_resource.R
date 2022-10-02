resource_specialisation_resource <- function(log) {
	absolute <- NULL
	relative <- NULL

	log %>%
		distinct(!!resource_id_(log), !!activity_id_(log)) %>%
		group_by(!!resource_id_(log)) %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_activities(log))
	}
