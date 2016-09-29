

throughput_time_case <- function(eventlog, units = "days") {
	stop_eventlog(eventlog)

	case_classifier <- case_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"


	e <- data.table::as.data.table(eventlog)

	min <- e[, .(min = min(timestamp_classifier)), by = case_classifier]
	max <- e[, .(max = max(timestamp_classifier)), by = case_classifier]
	e <- merge(min, max, by = "case_classifier")[, .(case_classifier, dur = max - min)]

	e <- e[, .(case_classifier,throughput_time = as.double(dur, units = units)),]
	e %>%
		tbl_df %>%
		arrange(-throughput_time)  -> e

	colnames(e)[colnames(e) == "case_classifier"] <- case_id(eventlog)


	return(e)


}
