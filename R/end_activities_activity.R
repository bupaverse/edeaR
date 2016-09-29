
end_activities_activity <- function(eventlog) {

	stop_eventlog(eventlog)

	eventlog %>%
		rename_("event_classifier" = activity_id(eventlog),
				"timestamp_classifier" = timestamp(eventlog)) %>%
		group_by_(case_id(eventlog)) %>%
		arrange(timestamp_classifier) %>%
		summarize(last_event = last(event_classifier)) %>%
		group_by(last_event) %>%
		summarize(absolute = n()) %>%
		ungroup() %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative))-> r

	colnames(r)[colnames(r) == "last_event"] <- activity_id(eventlog)
	return(r)

}
