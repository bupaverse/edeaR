resource_involvement_case <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by(!!case_id_(eventlog),  !!resource_id_(eventlog)) %>%
		summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(eventlog))
}
