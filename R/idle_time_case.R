idle_time_case <- function(eventlog, units) {

	colnames(eventlog)[colnames(eventlog) == lifecycle_id(eventlog)] <- "lifecycle_identifier"
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"


	e <- as.data.frame(eventlog)

	e %>%
		mutate(activate = ifelse(lifecycle_identifier == "start", 1, ifelse(lifecycle_identifier == "complete", -1, 0))) %>%
		group_by_(case_id(eventlog)) %>%
		arrange_("timestamp_classifier") %>%
		mutate(active = cumsum(activate),
			   next_ts = lead(timestamp_classifier)) %>%
		mutate(dur = as.double(next_ts - timestamp_classifier, units = units)) %>%
		filter(active == 0 & !is.na(dur)) %>%
		summarize(idle_time = sum(dur)) %>%
		return()

}
