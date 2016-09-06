traces_light <- function(eventlog){

	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"


	traces <- eventlog %>%
		group_by(case_classifier, activity_instance_classifier, event_classifier) %>%
		summarize(timestamp_classifier = min(timestamp_classifier)) %>%
		group_by(case_classifier) %>%
		arrange(timestamp_classifier) %>%
		summarize(trace = paste(event_classifier, collapse = ",")) %>%
		group_by(trace) %>%
		summarize()

		return(traces)

}
