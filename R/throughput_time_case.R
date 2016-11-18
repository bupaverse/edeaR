

throughput_time_case <- function(eventlog, units = "days") {
	stop_eventlog(eventlog)

	case_classifier <- case_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"


	e <- eventlog %>%
		group_by(case_classifier) %>%
		summarize(min = min(timestamp_classifier),
				  max = max(timestamp_classifier)) %>%
		mutate(throughput_time = as.double(max - min, units = "days")) %>%
		arrange(-throughput_time)

	colnames(e)[colnames(e) == "case_classifier"] <- case_id(eventlog)


	return(e)


}
