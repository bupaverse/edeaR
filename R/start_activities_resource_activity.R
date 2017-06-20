
start_activities_resource_activity <- function(eventlog) {

	eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		mutate(rank = row_number(!!as.symbol(timestamp(eventlog)))) %>%
		filter(rank == 1) %>%
		group_by(!!as.symbol(resource_id(eventlog)), !!as.symbol(activity_id(eventlog))) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative))  %>%
	return

}
