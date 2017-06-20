
activity_frequency_activity <- function(eventlog) {

	stop_eventlog(eventlog)

	r <- eventlog %>%
		group_by(!!as.symbol(activity_id(eventlog))) %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/sum(absolute))

	return(r)
}
