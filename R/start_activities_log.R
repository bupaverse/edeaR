
start_activities_log <- function(log) {

	absolute <- NULL
	relative <- NULL
	log %>%
		start_activities_activity() %>%
		summarize(absolute = n(),
				  relative = n()/n_activities(log))
}
