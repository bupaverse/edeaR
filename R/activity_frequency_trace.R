activity_frequency_trace <- function(eventlog) {
	cases <- case_list(eventlog)
	eventlog %>%
		activity_frequency_case() %>%
		merge(cases) %>%
		select(4,2,3) %>%
		unique
}
