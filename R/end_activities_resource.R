
end_activities_resource <- function(eventlog) {


	eventlog %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		mutate(rank = row_number(desc(!!as.symbol((timestamp(eventlog)))))) %>%
		filter(rank == 1) %>%
		group_by(!!as.symbol(resource_id(eventlog))) %>%
		summarize(absolute = n()) %>%
		arrange(desc(absolute)) %>%
		mutate(relative = absolute/n_cases(eventlog),
			   cum_sum = cumsum(relative)) %>%
		return()

}
