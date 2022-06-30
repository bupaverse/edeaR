
trace_length_case <- function(log) {

	log %>%
		group_by_case %>%
		summarize(absolute = n_distinct(!!activity_instance_id_(log)))
}
