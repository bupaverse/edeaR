
processing_time_resource <- function(eventlog,
									 units = "days",
									 raw) {
	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	case_classifier <- case_id(eventlog)
	event_classifier <- activity_id(eventlog)
	resource_classifier <- resource_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)


	res <- eventlog %>% count_(resource_classifier) %>% select_(resource_classifier)
	cas <- eventlog %>% count_(case_classifier) %>% select_(case_classifier)

	res_cas_pair_list <- merge(res,cas)


	r <- eventlog %>%
		group_by_(resource_classifier, case_classifier, activity_instance_classifier) %>%
		summarize(s = min(timestamp_classifier), e = max(timestamp_classifier)) %>%
		mutate(dur = as.double(e - s, units = units)) %>%
		summarize(dur = sum(dur)) %>%
		merge(res_cas_pair_list, all.y = T) %>%
		mutate(dur = ifelse(is.na(dur),0,dur)) %>%
		rename(processing_time = dur)

	if(raw == T) {
		return(r)
	}

	r %>%
		group_by_(resource_classifier) %>%
		summarize(min = min(processing_time),
				  q1 = quantile(processing_time, 0.25),
				  median = median(processing_time),
				  mean = mean(processing_time),
				  q3 = quantile(processing_time, probs = c(0.75)),
				  max = max(processing_time),
				  st_dev = sd(processing_time),
				  iqr = quantile(processing_time, probs = c(0.75)) - quantile(processing_time,probs = c(0.25)),
				  tot = sum(processing_time)) -> r

	return(r)
}
