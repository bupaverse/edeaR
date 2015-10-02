#' @title Generic print function for eventlog
#'
#' @export print.eventlog

print.eventlog <- function(x, ...) {
	log <- x
	cat("Event log consisting of:\n")
	cat(paste(nrow(x), "events\n", sep = " "))
	cat(paste(number_of_traces(x)$absolute[1], "traces\n", sep = " "))
	cat(paste(nrow(cases_light(x)), "cases\n", sep = " "))
	cat(paste(nrow(activities(x)), "activities\n", sep = " "))
	cat(paste(nrow(group_by_(x, activity_instance_id(x)) %>% summarize()), "activity instances\n\n", sep = " "))
	NextMethod(x)
}
