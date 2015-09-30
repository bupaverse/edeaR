#' @title Durations
#'
#' @description Computes the throughput times of each case.
#' Throughput time is defined as the interval between the start of the first event and the completion of the last event.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param units The time unit in which the throughput times should be reported.
#'
#'
#' @examples
#'
#'
#' data(example_log)
#' durations(example_log)
#'
#' @export durations

durations <- function(eventlog,
					  units = "days") {
	stop_eventlog(eventlog)
	library(lubridate)
	library(dplyr)

	colnames(eventlog)[colnames(eventlog)==life_cycle_id(eventlog)] <- "life_cycle_classifier"
	colnames(eventlog)[colnames(eventlog)==case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog)==timestamp(eventlog)] <- "timestamp_classifier"

	e <- eventlog

	s_durations <- e %>% filter(life_cycle_classifier == "start") %>% group_by(case_classifier) %>% summarize(s = min(timestamp_classifier))
	e_durations <- e %>% filter(life_cycle_classifier == "complete") %>% group_by(case_classifier) %>% summarize(e = max(timestamp_classifier))

	durations <- merge(s_durations, e_durations)
	durations$duration <- durations$e - durations$s
	durations$duration <- as.double(durations$duration, units = units)

	durations <- durations %>% select(case_classifier, duration)

	colnames(durations)[colnames(durations)=="case_classifier"] <- case_id(eventlog)
	colnames(durations)[colnames(durations)=="duration"] <- paste("duration_in_", units, sep ="")

	return(durations)

}
