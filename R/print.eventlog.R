#' @title Generic print function for eventlog
#'
#' @export print.eventlog

print.eventlog <- function(x, ...) {
	log <- x
	cat("Event log consisting of:\n")
	cat(paste(n_events(x), "events\n", sep = " "))
	cat(paste(nrow(traces(x)), "traces\n", sep = " "))
	cat(paste(n_cases(x), "cases\n", sep = " "))
	cat(paste(n_activities(x), "activities\n", sep = " "))
	cat(paste(n_activity_instances(x), "activity instances\n\n", sep = " "))
	NextMethod(x)
}
