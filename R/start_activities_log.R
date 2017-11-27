

start_activities_log <- function(eventlog) {
	absolute <- NULL
	relative <- NULL
	eventlog %>%
		start_activities_activity %>%
		summarize(absolute = n(),
				  relative = n()/n_activities(eventlog))
}
