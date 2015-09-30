#' @title Generic summary function for eventlog class
#' @export summary.eventlog

summary.eventlog <- function(object, ...){

	library(dplyr)
		eventlog <- object

		log_traces <- traces(eventlog)

		number_of_events <- nrow(eventlog)
		number_of_cases <- sum(log_traces$absolute_frequency)
		number_of_traces <- nrow(log_traces)
		number_of_activities <- nrow(activities(eventlog))


		colnames(eventlog)[colnames(eventlog)==timestamp(eventlog)] <- "timestamp_classifier"

		first_event <- as.character(arrange(eventlog, timestamp_classifier)$timestamp_classifier[1])
		last_event <- as.character(arrange(eventlog, timestamp_classifier)$timestamp_classifier[nrow(eventlog)])

		events_per_case <- number_of_events/number_of_cases



		cat("Number of events:  ")
		cat(number_of_events)
		cat("\nNumber of cases:  ")
		cat(number_of_cases)
		cat("\nNumber of traces:  ")
		cat(number_of_traces)
		cat("\nNumber of activities:  ")
		cat(number_of_activities)
		cat("\nAverage trace length:  ")
		cat(events_per_case)
		cat("\n\nStart eventlog:  ")
		cat(first_event)
		cat("\nEnd eventlog:  ")
		cat(last_event)
}
