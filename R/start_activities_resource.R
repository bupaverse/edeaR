
start_activities_resource <- function(eventlog) {
	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by_case %>%
		mutate(row_number(!!timestamp_(eventlog))==1) %>%
		group_by_resource %>%
		summarize(absolute = n()) %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative)) %>%
		return()

}
