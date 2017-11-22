

filter_endpoints_sets <- function(eventlog,
							 start_activities,
							 end_activities,
							 reverse) {

	first_activity <- NULL
	last_activity <- NULL

	if(is.null(start_activities))
		start_activities <- eventlog %>% activities %>% pull(1)
	if(is.null(end_activities))
		end_activities <- eventlog %>% activities %>% pull(1)


	case_selection <-  eventlog %>%
		cases %>%
		filter(first_activity %in% start_activities, last_activity %in% end_activities) %>%
		pull(1)

	filter_case(eventlog, case_selection, reverse)
}
