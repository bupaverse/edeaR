
processing_time_activity <- function(eventlog,
									 units,
									 raw) {
	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	event_classifier <- activity_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)

	r <- eventlog %>%
		group_by_(event_classifier, activity_instance_classifier) %>%
		summarize(s = min(timestamp_classifier), e = max(timestamp_classifier)) %>%
		mutate(processing_time = as.double(e - s, units = units))

	if(raw == T) {
		return(r)
	}
	else {
		r <- r %>%
			summarize(relative_frequency = n(),
					  min = min(processing_time),
					  q1 = quantile(processing_time, probs = c(0.25)),
					  median = median(processing_time),
					  mean = mean(processing_time),
					  q3 = quantile(processing_time, probs = c(0.75)),
					  max = max(processing_time),
					  st_dev = sd(processing_time),
					  iqr = quantile(processing_time, probs = c(0.75)) - quantile(processing_time,probs = c(0.25)),
					  tot = sum(processing_time)) %>%
			mutate(relative_frequency = relative_frequency/sum(relative_frequency)) %>%
			arrange(desc(relative_frequency))

		return(r)
	}
}
