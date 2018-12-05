

resource_involvement_resource_activity <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog), !!case_id_(eventlog)) %>%
		summarize() %>%
		summarize("absolute" = n_distinct(!!case_id_(eventlog))) %>%
		ungroup() %>%
		mutate(relative = absolute/n_cases(eventlog))

}
