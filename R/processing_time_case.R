

processing_time_case <- function(eventlog,
								 units = "days") {
	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"

	case_classifier <- case_id(eventlog)
	event_classifier <- activity_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)

	e <- eventlog %>%
		group_by_(case_classifier, activity_instance_classifier) %>%
		mutate(r = row_number(timestamp_classifier),
			   min_r = min(r),
			   max_r = max(r),
			   endpoints = ifelse(r == min_r, "first", ifelse(r == max_r, "last","none"))) %>%
		filter(endpoints %in% c("first","last")) %>%
		ungroup() %>%
		select(-r, -min_r, -max_r) %>%
		select_(case_classifier, activity_instance_classifier, "timestamp_classifier", "endpoints") %>%
		spread(endpoints, timestamp_classifier) %>%
		mutate(dur = as.double(last - first, units = units)) %>%
		group_by_(case_classifier) %>%
		summarize(tot_dur = sum(dur)) %>%
		select_(case_classifier, "tot_dur") %>%
		rename(processing_time = tot_dur) %>%
		arrange(desc(processing_time))

	return(e)

}
