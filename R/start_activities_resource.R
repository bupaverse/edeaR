
start_activities_resource <- function(log) {

	absolute <- NULL
	relative <- NULL

	log %>%
		as.data.frame() %>%
		group_by(!!case_id_(log)) %>%
		arrange(!!as.symbol(timestamp(log)), .order) %>%
		summarize(!!as.symbol(activity_id(log)) := first(!!as.symbol(activity_id(log))),
				  !!as.symbol(resource_id(log)) := first(!!as.symbol(resource_id(log)))) %>%
		group_by(!!as.symbol(resource_id(log))) %>%
		summarize(absolute = n()) %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(log),
			   cum_sum = cumsum(relative)) %>%
		arrange(!!resource_id_(log))
}
