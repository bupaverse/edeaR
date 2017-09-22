
activity_frequency_activity <- function(eventlog) {

	stop_eventlog(eventlog)

	r <- eventlog %>%
		group_by(!!as.symbol(activity_id(eventlog))) %>%
		summarize(absolute = n_distinct(!!as.symbol(activity_instance_id(eventlog)))) %>%
		mutate(relative = absolute/sum(absolute))

	return(r)
}
