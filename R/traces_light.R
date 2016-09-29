traces_light <- function(eventlog){

	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"

	eDT <- data.table::data.table(eventlog)

	cases <- eDT[,
				 .(timestamp_classifier = min(timestamp_classifier)),
				 by = .(case_classifier, activity_instance_classifier,  event_classifier)]

	cases <- cases[order(timestamp_classifier), .(trace = paste(event_classifier, collapse = ",")),
				   by = .(case_classifier)]

	#	cases <- eventlog %>%
	#		group_by(case_classifier, activity_instance_classifier, event_classifier) %>%
	#		summarize(timestamp_classifier = min(timestamp_classifier)) %>%
	#		group_by(case_classifier) %>%
	#		arrange(timestamp_classifier) %>%
	#		summarize(trace = paste(event_classifier, collapse = ",")) %>%
	#		mutate(trace_id = as.numeric(factor(trace)))


	casesDT <- data.table(cases)

	traces <- casesDT[, .(absolute_frequency = .N), by = .(trace)]

	traces <- traces[order(absolute_frequency, decreasing = T),relative_frequency:=absolute_frequency/sum(absolute_frequency)]
	traces <- tbl_df(traces)

	#traces <- eventlog %>%
	#	group_by(case_classifier, activity_instance_classifier, event_classifier) %>%
	#	summarize(timestamp_classifier = min(timestamp_classifier)) %>%
	#	group_by(case_classifier) %>%
	#	arrange(timestamp_classifier) %>%
	#	summarize(trace = paste(event_classifier, collapse = ",")) %>%
	#	group_by(trace) %>%
	#	summarize()

		return(traces)

}
