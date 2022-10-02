

resource_involvement_resource_activity <- function(log) {

	absolute <- NULL
	relative <- NULL

	log %>%
		distinct(!!resource_id_(log), !!activity_id_(log), !!case_id_(log)) %>%
		group_by(!!resource_id_(log)) %>%
		summarize("absolute" = n_distinct(!!case_id_(log))) %>%
		ungroup() %>%
		mutate(relative = absolute/n_cases(log))

}
