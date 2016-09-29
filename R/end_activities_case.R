end_activities_case <- function(eventlog) {

	case_classifier <- case_id(eventlog)
	timestamp_classifier <- timestamp(eventlog)
	event_classifier <- activity_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp"

	eventlog %>%
		group_by_(case_classifier) %>%
		mutate(r = row_number(desc(timestamp))) %>%
		filter(r == 1) %>%
		select_(case_classifier, "end_activity" = "event_classifier") %>%
		return()

}
