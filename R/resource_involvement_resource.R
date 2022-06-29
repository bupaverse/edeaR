
resource_involvement_resource <- function(eventlog) {
	relative <- NULL
	absolute <- NULL

	eventlog %>%
		as.data.frame() %>%
		distinct(!!resource_id_(eventlog), !!case_id_(eventlog)) %>%
		group_by(!!resource_id_(eventlog)) %>%
		summarize(absolute = n_distinct(!!case_id_(eventlog))) %>%
		mutate(relative = absolute/n_cases(eventlog))
}
