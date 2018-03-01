
end_activities_activity <- function(eventlog) {
	absolute <- NULL
	relative <- NULL

	eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		arrange(!!as.symbol(timestamp(eventlog)), .order) %>%
		summarize(!!as.symbol(activity_id(eventlog)) := last(!!as.symbol(activity_id(eventlog)))) %>%
		group_by(!!as.symbol(activity_id(eventlog))) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative))
}
