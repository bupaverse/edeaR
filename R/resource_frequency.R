#' Metric: Resource frequency
#'
#' Analyses the frequency of resources at different levels of analysis
#'
#' Comparable to the concept of the activity frequency the frequency of resources in a business process can
#' also be very insightful for companies, e.g., during company restructuring.
#'
#' \itemize{
#' \item On the level of the complete event log, summary statistics show the number of times
#' a resource executes an activity in the complete event log.
#'
#' \item To get a better view on the variance between the different cases,
#' the summary statistics of the frequency of resources can be
#' calculated on the level of the cases. This way, a company gets an insight in the
#' number of different resources working on each case together with the number
#' of activities a resource executes per case.
#'
#' \item At the level of the different activities, the
#' resource frequency states how many different resources are executing a specific
#' activity in the complete event log.
#'
#' \item At the level of the distinct resources in
#' the event log, this metric simply shows the absolute and relative frequency of
#' occurrences of each resource in the complete event log.
#'
#' \item Finally, at the most specific level of analysis, the absolute and relative number of times each resource-activity
#' level occurs in the complete event log can be calculated. Two different relative
#' numbers are provided here, one from the resource perspective and one from
#' the activity perspective. At the resource perspective, the denominator is the
#' total number of executions by the resource under consideration. At the activity
#' perspective, the denominator is the total number of occurrences of the activity
#' under consideration.
#' }
#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#'
#' @param sort Sort output on count. Defaults to TRUE. Only for levels with frequency count output.
#'
#' @export resource_frequency


resource_frequency <- function(eventlog, level, append, ...) {
	UseMethod("resource_frequency")
}

#' @describeIn resource_frequency Resource frequency for eventlog
#' @export


resource_frequency.eventlog <- function(eventlog,
										level = c("log","case","activity","resource","resource-activity"),
										append = FALSE,
										append_column = NULL,
										sort = TRUE,
										...) {

	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   level == "activity"~"median",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = resource_frequency_log,
				  case = resource_frequency_case,
				  activity = resource_frequency_activity,
				  resource = resource_frequency_resource,
				  "resource-activity" = resource_frequency_resource_activity)

	output <- FUN(eventlog = eventlog)

	if(level %in% c("resource","resource-activity") && sort) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(eventlog, output, level, append,append_column, "resource_frequency", ifelse(level == "resource", 2,
																				ifelse(level == "resource-activity", 3,9)))


}


#' @describeIn resource_frequency Resource frequency for grouped eventlog
#' @export

resource_frequency.grouped_eventlog <- function(eventlog,
												level = c("log","case","activity","resource","resource-activity"),
												append = FALSE,
												append_column = NULL,
												sort = TRUE,
												...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   level == "activity"~"median",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = resource_frequency_log,
				  case = resource_frequency_case,
				  activity = resource_frequency_activity,
				  resource = resource_frequency_resource,
				  "resource-activity" = resource_frequency_resource_activity)

	if(!(level %in% c("log"))) {
		grouped_metric(eventlog, FUN) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN)  -> output
	}
	if(level %in% c("resource","resource-activity") && sort) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append,append_column, "resource_frequency", ifelse(level == "resource", 2,
																				ifelse(level == "resource-activity", 3,9)))
}
