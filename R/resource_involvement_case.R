resource_involvement_case <- function(log) {

	absolute <- NULL
	relative <- NULL

	log %>%
		distinct(!!case_id_(log),  !!resource_id_(log)) %>%
		group_by(!!case_id_(log)) %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/n_resources(log))
}
