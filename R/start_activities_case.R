start_activities_case <- function(eventlog) {

	eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		mutate(r = row_number(!!as.symbol(timestamp(eventlog)))) %>%
		filter(r == 1) %>%
		select(!!as.symbol(case_id(eventlog)), start_activity = !!as.symbol(activity_id(eventlog))) %>%
		return()

}
