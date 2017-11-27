start_activities_case <- function(eventlog) {

	eventlog %>%
		group_by_case %>%
		filter(row_number(!!timestamp_(eventlog)) == 1) %>%
		select(!!case_id_(eventlog), start_activity = !!activity_id_(eventlog))

}
