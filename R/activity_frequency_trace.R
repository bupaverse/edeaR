activity_frequency_trace <- function(eventlog) {

	stop_eventlog(eventlog)

	cases <- cases_light(eventlog)

	eventlog %>%
		activity_frequency_case() %>%
		merge(cases) %>%
		select(trace, absolute, relative) %>%
		unique -> r


	return(r)


}
