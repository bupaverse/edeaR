resource_involvement_case <- function(log) {

	absolute <- NULL
	relative <- NULL

	log %>%
		group_by(!!case_id_(log),  !!resource_id_(log)) %>%
		#summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(log))
}
