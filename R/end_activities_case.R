end_activities_case <- function(eventlog) {
	eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		summarize(!!as.symbol(activity_id(eventlog)) := last(!!as.symbol(activity_id(eventlog)))) %>%
		select(!!as.symbol(case_id(eventlog)), end_activity = !!as.symbol(activity_id(eventlog)))

}
