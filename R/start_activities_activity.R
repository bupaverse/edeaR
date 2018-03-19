
start_activities_activity <- function(eventlog) {

	first_event <- NULL
	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by_case %>%
		arrange(!!timestamp_(eventlog), .order) %>%
		summarize(first_event = first(!!activity_id_(eventlog))) %>%
		group_by(first_event) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative)) -> r

	colnames(r)[colnames(r) == "first_event"] <- activity_id(eventlog)
	return(r)

}
