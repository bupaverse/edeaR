

processing_time_case <- function(eventlog,
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


	r <- select(r, case_classifier, tot_dur) %>% rename(processing_time = tot_dur) %>% arrange(desc(processing_time))

	return(r)

}
