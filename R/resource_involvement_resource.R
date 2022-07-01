
resource_involvement_resource <- function(log) {
	relative <- NULL
	absolute <- NULL

	log %>%
		as.data.frame() %>%
		distinct(!!resource_id_(log), !!case_id_(log)) %>%
		group_by(!!resource_id_(log)) %>%
		summarize(absolute = n_distinct(!!case_id_(log))) %>%
		mutate(relative = absolute/n_cases(log))
}
