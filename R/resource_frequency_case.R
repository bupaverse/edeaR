resource_frequency_case <- function(log) {

	nr_of_resources <- NULL

	log %>%
		distinct(!!case_id_(log), !!resource_id_(log), !!activity_instance_id_(log)) %>%
		group_by(!!case_id_(log), !!resource_id_(log)) %>%
		summarize(freq = n()) %>%
		group_by(!!case_id_(log)) %>%
		grouped_summary_statistics("freq", nr_of_resources = n()) %>%
		select(!!case_id_(log), nr_of_resources, everything())

}
