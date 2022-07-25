resource_specialisation_case <- function(log) {

	freq <- NULL
	nr_of_activity_types <- NULL

	log %>%
		group_by(!!resource_id_(log), !!activity_id_(log), !!case_id_(log)) %>%
		summarize() %>%
		group_by(!!case_id_(log)) %>%
		mutate(nr_of_activity_types = n_distinct(!!activity_id_(log))) %>%
		group_by(!!case_id_(log), nr_of_activity_types, !!resource_id_(log)) %>%
		summarize(freq = n()) %>%
		grouped_summary_statistics("freq")
}
