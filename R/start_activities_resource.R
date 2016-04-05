

start_activities_resource <- function(eventlog) {

	stop_eventlog(eventlog)

	resource_classifier <- resource_id(eventlog)
	case_classifier <- case_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"

	e <- eventlog %>%
		group_by_(case_classifier) %>%
		mutate(rank = row_number(timestamp_classifier),
			   min_rank = min(rank)) %>%
		filter(rank == min_rank) %>% ungroup()

	ncases <- n_cases(eventlog)

	r <- e %>% group_by_(resource_classifier) %>% summarize(absolute = n()) %>% arrange(desc(absolute))
	r$relative <- r$absolute/ncases
	r$cum_sum <- cumsum(r$relative)
	return(r)

}
