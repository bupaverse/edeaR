

number_of_selfloops_activity <- function(eventlog) {

	stop_eventlog(eventlog)

	sl <- selfloops(eventlog)
	tr <- traces(eventlog)
	r <- merge(sl, tr, "trace")

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"

	r <- group_by(r, event_classifier) %>% summarize(absolute = sum(absolute_frequency))

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	return(r)

}
