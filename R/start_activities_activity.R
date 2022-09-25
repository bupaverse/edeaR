
start_activities_activity <- function(log) {

	first_event <- NULL
	absolute <- NULL
	relative <- NULL

	log %>%
		as.data.frame() %>%
		group_by(!!case_id_(log)) %>%
		arrange(!!timestamp_(log), .order) %>%
		summarize(first_event = first(!!activity_id_(log))) %>%
		group_by(first_event) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(log),
			   cum_sum = cumsum(relative)) -> r

	colnames(r)[colnames(r) == "first_event"] <- activity_id(log)
	return(r)

}
