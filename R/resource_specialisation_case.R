resource_specialisation_case <- function(eventlog) {

	freq <- NULL
	nr_of_activity_types <- NULL

	eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog), !!case_id_(eventlog)) %>%
		summarize() %>%
		group_by(!!case_id_(eventlog)) %>%
		mutate(nr_of_activity_types = n_distinct(!!activity_id_(eventlog))) %>%
		group_by(!!case_id_(eventlog), nr_of_activity_types, !!resource_id_(eventlog)) %>%
		summarize(freq = n()) %>%
		grouped_summary_statistics("freq")
}
