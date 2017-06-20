
processing_time_resource_activity <- function(eventlog,
											  units = "days") {
	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	case_classifier <- case_id(eventlog)
	event_classifier <- activity_id(eventlog)
	resource_classifier <- resource_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)


	res <- eventlog %>% count_(resource_classifier) %>% select_(resource_classifier)
	cas <- eventlog %>% count_(case_classifier) %>% select_(case_classifier)
	act <- eventlog %>% count_(event_classifier) %>% select_(event_classifier)

	res_cas_pair_list <- merge(res,merge(cas,act))


	r <- eventlog %>%
		group_by_(resource_classifier, case_classifier, event_classifier, activity_instance_classifier) %>%
		summarize(s = min(timestamp_classifier), e = max(timestamp_classifier)) %>%
		mutate(dur = as.double(e - s, units = units)) %>%
		summarize(dur = sum(dur), n = n()) %>%
		merge(res_cas_pair_list, all.y = T) %>%
		mutate(dur = ifelse(is.na(dur),0,dur),
			   n = ifelse(is.na(n),0,n)) %>%
		group_by_(resource_classifier, event_classifier) %>%
		mutate(present = n > 0) %>%
		mutate(cnt = sum(present))%>%
		filter(cnt != 0) %>%
		select(-n,-cnt, -present) %>%
		rename(processing_time = dur)

	raw <- r

	r %>%
		group_by_(resource_classifier, event_classifier) %>%
		summarize(min = min(processing_time),
				  q1 = quantile(processing_time, probs = c(0.25)),
				  median = median(processing_time),
				  mean = mean(processing_time),
				  q3 = quantile(processing_time, probs = c(0.75)),
				  max = max(processing_time),
				  st_dev = sd(processing_time),
				  iqr = quantile(processing_time, probs = c(0.75)) - quantile(processing_time,probs = c(0.25)),
				  tot = sum(processing_time)) -> r

	attr(r, "raw") <- raw


	return(r)
}
