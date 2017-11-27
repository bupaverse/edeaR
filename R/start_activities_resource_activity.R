
start_activities_resource_activity <- function(eventlog) {
	absolute <- NULL
	relative <- NULL
	eventlog %>%
		group_by_case %>%
		mutate(row_number(!!timestamp_(eventlog))  == 1) %>%
		group_by_resource_activity() %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative))

}
