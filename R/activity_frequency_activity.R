
activity_frequency_activity <- function(eventlog) {
	absolute <- NULL
	eventlog %>%
		group_by(!!as.symbol(activity_id(eventlog))) %>%
		summarize(absolute = n_distinct(!!as.symbol(activity_instance_id(eventlog)))) %>%
		mutate(relative = absolute/sum(absolute))

}
