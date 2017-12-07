start_activities_case <- function(eventlog) {

	eventlog %>%
		group_by(!!case_id_(eventlog)) %>%
		summarize(!!activity_id_(eventlog) := first(!!activity_id_(eventlog))) %>%
		select(!!case_id_(eventlog), start_activity = !!activity_id_(eventlog))

}
