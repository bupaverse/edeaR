
resource_frequency_resource <- function(log) {
	absolute <- NULL
	relative <- NULL

	log %>%
		group_by(!!resource_id_(log), !!activity_instance_id_(log)) %>%
		#summarize() %>%
		summarize(absolute = n()) %>%
		mutate(relative = absolute/sum(absolute))

}
