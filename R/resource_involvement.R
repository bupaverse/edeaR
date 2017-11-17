#' @title Metric: Resource Involvement
#'
#' @description Calculates for each resource/resource-activity in what percentage of cases it is present.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis The level of analysis: resource or resource-activity.
#' @param append Logical indicating whether result should be append to original event log
#' @param ...
#'
#' @export resource_involvement

resource_involvement <- function(eventlog, level, append, ...) {
	UseMethod("resource_involvement")
}

#' @describeIn resource_involvement Resource involvement for eventlog
#' @export

resource_involvement.eventlog <- function(eventlog, level = c("case","resource","resource-activity"), append = F, ...) {

	mapping <- mapping(eventlog)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  case = resource_involvement_case,
				  resource = resource_involvement_resource,
				  "resource-activity" = resource_involvement_resource_activity)

	output <- FUN(eventlog = eventlog)

	return_metric(eventlog, output, level, append, "resource_involvement",2)
}

#' @describeIn resource_involvement Resource involvement for grouped eventlog
#' @export

resource_involvement.grouped_eventlog <- function(eventlog, level = c("case","resource","resource-activity"), append = F, ...) {

	mapping <- mapping(eventlog)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  case = resource_involvement_case,
				  resource = resource_involvement_resource,
				  "resource-activity" = resource_involvement_resource_activity)

	output <- grouped_metric(eventlog, FUN)

	return_metric(eventlog, output, level, append, "resource_involvement",2)
}
