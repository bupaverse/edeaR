activity_frequency_case <- function(eventlog) {

	stop_eventlog(eventlog)

	s <- eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		summarize(absolute = n_distinct(!!as.symbol(activity_id(eventlog))),
				  relative = absolute/n())

	return(s)


}
