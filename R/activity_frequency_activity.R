
activity_frequency_activity <- function(log) {
	absolute <- NULL
	log %>%
		group_by(!!as.symbol(activity_id(log))) %>%
		summarize(absolute = n_distinct(!!as.symbol(activity_instance_id(log)))) %>%
		mutate(relative = absolute/sum(absolute))

}
