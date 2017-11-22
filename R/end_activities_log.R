

end_activities_log <- function(eventlog) {
	absolute <- NULL
	relative <- NULL
	eventlog %>%
		end_activities_activity %>%
		summarize(absolute = n(),
				  relative = n()/n_activities(eventlog))
}
