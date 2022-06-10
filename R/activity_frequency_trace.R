activity_frequency_trace <- function(log) {
	cases <- case_list(log)
	log %>%
		activity_frequency_case() %>%
		merge(cases) %>%
		arrange(trace) %>%
		select(4,2,3) %>%
		unique
}
