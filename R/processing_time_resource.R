
processing_time_resource <- function(eventlog,
									 units = "days") {
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
		group_by_(resource_classifier) %>%
		summarize(min = min(dur),
				  q1 = quantile(dur, probs = c(0.25)),
				  median = median(dur),
				  mean = mean(dur),
				  q3 = quantile(dur, probs = c(0.75)),
				  max = max(dur),
				  st_dev = sd(dur),
				  iqr = quantile(dur, probs = c(0.75)) - quantile(dur,probs = c(0.25)),
				  tot = sum(dur))

	return(r)
}
