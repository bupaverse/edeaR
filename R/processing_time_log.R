

processing_time_log <- function(eventlog,
								units = "days") {
	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"

	e <- eventlog %>% group_by(case_classifier, event_classifier, activity_instance_classifier) %>% summarize(s = min(timestamp_classifier), e = max(timestamp_classifier))

	e$dur <- e$e - e$s

	e$dur  <- as.double(e$dur , units = units)

	r <- e %>% group_by(case_classifier) %>% summarize(tot_dur = sum(dur))

	s <- summary(r$tot_dur)
	s <- c(s, St.Dev = sd(r$tot_dur))
	s <- c(s, IQR = s[5] - s[2])
	names(s) <- c("min","q1","median","mean","q3","max","st_dev","iqr")

	s <- as.data.frame(s)
	s <- t(s)
	row.names(s) <- NULL
	return(s)
}
