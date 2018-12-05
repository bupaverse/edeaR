
start_activities_resource <- function(eventlog) {
	absolute <- NULL
	relative <- NULL
	eventlog %>%
		group_by_case() %>%
		arrange(!!as.symbol(timestamp(eventlog)), .order) %>%
		summarize(!!as.symbol(activity_id(eventlog)) := first(!!as.symbol(activity_id(eventlog))),
				  !!as.symbol(resource_id(eventlog)) := first(!!as.symbol(resource_id(eventlog)))) %>%
		group_by(!!as.symbol(resource_id(eventlog))) %>%
		summarize(absolute = n()) %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative)) %>%
		arrange(!!activity_id_(eventlog))


}
