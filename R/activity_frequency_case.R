activity_frequency_case <- function(eventlog) {
	absolute <- NULL
	relative <- NULL
	n_labels <- NULL
	n_instances <- NULL


	eventlog %>%
		group_by(!!case_id_(eventlog), !!activity_id_(eventlog), !!activity_instance_id_(eventlog)) %>%
		summarize() %>%
		summarize(n = n()) %>%
		summarize(n_instances = sum(n), n_labels = n()) %>%
		transmute(!!case_id_(eventlog),
				  absolute = n_labels,
				  relative = n_labels/n_instances)
}
