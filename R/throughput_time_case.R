

throughput_time_case <- function(eventlog, units) {
	stop_eventlog(eventlog)

	case_classifier <- case_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"

	e <- eventlog %>%
		as.data.table %>%
		.[, .(min = min(timestamp_classifier),
			  max = max(timestamp_classifier)), .(case_classifier)] %>%
		.[, .(throughput_time = difftime(max, min, units = units)), .(case_classifier)]

	colnames(e)[colnames(e) == "case_classifier"] <- case_id(eventlog)


	return(e)


}
