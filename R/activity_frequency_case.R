activity_frequency_case <- function(eventlog) {
	absolute <- NULL
	eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		summarize(absolute = n_distinct(!!as.symbol(activity_id(eventlog))),
				  relative = absolute/n_distinct(!!as.symbol(activity_instance_id(eventlog))))
}
