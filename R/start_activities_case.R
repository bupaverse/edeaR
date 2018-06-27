start_activities_case <- function(eventlog) {

	eventlog %>%
		group_by_case() %>%
		arrange(!!as.symbol(timestamp(eventlog)), .order) %>%
		summarize(!!as.symbol(activity_id(eventlog)) := first(!!as.symbol(activity_id(eventlog)))) %>%
		select(!!as.symbol(case_id(eventlog)), !!as.symbol(activity_id(eventlog)))
}
