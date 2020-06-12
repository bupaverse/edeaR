start_activities_case <- function(eventlog) {

	eventlog %>%
		as.data.frame() %>%
		group_by(!!case_id_(eventlog)) %>%
		arrange(!!as.symbol(timestamp(eventlog)), .order) %>%
		summarize(!!as.symbol(activity_id(eventlog)) := first(!!as.symbol(activity_id(eventlog)))) %>%
		select(!!as.symbol(case_id(eventlog)), !!as.symbol(activity_id(eventlog)))
}
