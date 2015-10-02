
processing_time_activity <- function(eventlog,
							  units = "days") {
	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == life_cycle_id(eventlog)] <- "life_cycle_classifier"
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"


	e <- eventlog %>% group_by(event_classifier, activity_instance_classifier) %>% summarize(s = min(timestamp_classifier), e = max(timestamp_classifier))


	e$dur <- e$e - e$s

	e$dur  <- as.double(	e$dur , units = units)

	r <- e %>% group_by(event_classifier) %>% summarize(relative_frequency = n(),
															   min = min(dur),
															   q1 = quantile(dur, probs = c(0.25)),
															   median = median(dur),
															   mean = mean(dur),
															   q3 = quantile(dur, probs = c(0.75)),
															   max = max(dur),
															   st_dev = sd(dur),
															   iqr = quantile(dur, probs = c(0.75)) - quantile(dur,probs = c(0.25))
	)


	r <- r %>% arrange(desc(relative_frequency))

	colnames(r)[colnames(r)=="event_classifier"] <- activity_id(eventlog)

	return(r)

}
