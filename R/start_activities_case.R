start_activities_case <- function(eventlog) {

	eventlog %>%
		group_by(!!case_id_(eventlog)) %>%
		arrange(!!timestamp_(eventlog), .order) %>%
		slice(1:1) %>%
		select(!!case_id_(eventlog), start_activity = !!activity_id_(eventlog))

}
