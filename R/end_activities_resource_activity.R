
end_activities_resource_activity <- function(eventlog) {
	absolute <- NULL
	relative <- NULL
	eventlog %>%
		group_by_case() %>%
		arrange(!!as.symbol(timestamp(eventlog)), .order) %>%
		summarize(!!as.symbol(activity_id(eventlog)) := last(!!as.symbol(activity_id(eventlog))),
				  !!as.symbol(resource_id(eventlog)) := last(!!as.symbol(resource_id(eventlog)))) %>%
		group_by(!!as.symbol(resource_id(eventlog)), !!as.symbol(activity_id(eventlog))) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative)) %>%
		arrange(!!resource_id_(eventlog), !!activity_id_(eventlog))
}
