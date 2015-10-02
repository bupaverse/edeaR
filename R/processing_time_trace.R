

processing_time_trace <- function(eventlog,
								  units = "days") {
	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == life_cycle_id(eventlog)] <- "life_cycle_classifier"
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"


	e <- eventlog %>% group_by(case_classifier, event_classifier, activity_instance_classifier) %>% summarize(s = min(timestamp_classifier), e = max(timestamp_classifier))

	e$dur <- e$e - e$s

	e$dur  <- as.double(e$dur , units = units)

	r <- e %>% group_by(case_classifier) %>% summarize(tot_dur = sum(dur))

	ca <- cases_light(eventlog)
	colnames(ca)[colnames(ca)==case_id(eventlog)] <- "case_classifier"

	r <- merge(ca,r)
	r <- select(r, trace, tot_dur)
	r <- r  %>% group_by(trace) %>% summarize(relative_frequency = n(),
															   min = min(tot_dur),
															   q1 = quantile(tot_dur, probs = c(0.25)),
															   median = median(tot_dur),
															   mean = mean(tot_dur),
															   q3 = quantile(tot_dur, probs = c(0.75)),
															   max = max(tot_dur),
															   st_dev = sd(tot_dur),
															   iqr = quantile(tot_dur, probs = c(0.75)) - quantile(tot_dur,probs = c(0.25)))

	r$relative_frequency <- r$relative_frequency / sum(r$relative_frequency)
	r <- r %>% arrange(desc(relative_frequency))

	return(r)

}
