

filter_endpoints_sets <- function(eventlog,
							 start_activities = NULL,
							 end_activities = NULL,
							 reverse) {

	if(is.null(start_activities))
		start_activities <- eventlog %>% activities %>% pull(1)
	if(is.null(end_activities))
		end_activities <- eventlog %>% activities %>% pull(1)

	c_sum <- cases(eventlog = eventlog)

	case_selection <-  c_sum %>%
		filter(first_activity %in% start_activities, last_activity %in% end_activities) %>%
		pull(1)

	filter_case(eventlog, case_selection, reverse)
}
