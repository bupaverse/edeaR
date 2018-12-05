
resource_involvement_resource <- function(eventlog) {
	relative <- NULL
	absolute <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!case_id_(eventlog)) %>%
		summarize() %>%
		summarize(absolute = n_distinct(!!case_id_(eventlog))) %>%
		mutate(relative = absolute/n_cases(eventlog))
}
