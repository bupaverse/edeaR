
end_activities_activity <- function(eventlog) {

	eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		arrange(!!as.symbol(timestamp(eventlog))) %>%
		summarize(last_event = last(!!as.symbol(activity_id(eventlog)))) %>%
		group_by(last_event) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative))-> r

	colnames(r)[colnames(r) == "last_event"] <- activity_id(eventlog)
	return(r)

}
