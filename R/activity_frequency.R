#' @title Activity Frequency
#'
#' @description Provides summary statistics about the frequency of activity types at the level of log, traces, cases, activity types.
#'
#' @param log \code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.).
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"} (default), \code{"trace"}, \code{"case"}, or \code{"activity"}.
#' For more information, see \code{vignette("metrics", "edeaR")} and 'Details' below.
#' @param sort \code{\link{logical}} (default \code{TRUE}): Sort output on count. Only for levels with frequency count output.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{log} level, this metric shows the summary statistics of the frequency of activities throughout the complete log.
#' \item On \code{case} level, this metric shows the absolute and relative number of times the different activity types occur in each case.
#' The absolute number shows the number of distinct activity types that occur in each of the cases.
#' The relative number is calculated based on the total activity executions in the case.
#' \item On \code{trace} level, this metric presents the absolute and relative number of times a specific activity type occurs in each trace.
#' \item On \code{activity} level, this metric provides the absolute and relative frequency of a specific activity in the complete log.
#' }
#'
#' @family metrics
#'
#' @concept metrics_structuredness
#'
#' @references Swennen, M. (2018). Using Event Log Knowledge to Support Operational Exellence Techniques (Doctoral dissertation). Hasselt University.
#'
#' @export activity_frequency
activity_frequency <- function(log,
							   level = c("log", "trace", "activity", "case"),
							   sort = TRUE) {
	UseMethod("activity_frequency")
}


#' @describeIn activity_frequency Computes the activity frequency for an \code{\link[bupaR]{eventlog}}.
#' @export
activity_frequency.eventlog <- function(log,
										level = c("log", "trace", "activity", "case"),
										sort = TRUE) {

	level <- rlang::arg_match(level)


	FUN <- switch(level,
				  log = activity_frequency_log,
				  case = activity_frequency_case,
				  trace = activity_frequency_trace,
				  activity = activity_frequency_activity)

	output <- FUN(log = log)

	if(sort && level %in% c("case", "trace", "activity")) {
		output %>%
			arrange(-.data[["absolute"]]) -> output
	}

	return_metric_v2(log, output, level, "activity_frequency")
}

#' @describeIn activity_frequency Computes the activity frequency for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
activity_frequency.grouped_eventlog <- function(log,
												level = c("log", "trace", "activity", "case"),
												sort = TRUE) {

	level <- rlang::arg_match(level)

	FUN <- switch(level,
				  log = activity_frequency_log,
				  case = activity_frequency_case,
				  trace = activity_frequency_trace,
				  activity = activity_frequency_activity)

	# Doesn't work!
	apply_grouped_fun(log, FUN, .keep_groups = FALSE, .returns_log = FALSE) -> output

	#if(level != "log") {
	#	grouped_metric(log, FUN) -> output
	#} else {
	#	grouped_metric_raw_log(log, FUN) -> output
	#}

	if(sort && level %in% c("case", "trace","activity")) {
		output %>%
			arrange(-.data[["absolute"]]) -> output
	}

	return_metric_v2(log, output, level, "activity_frequency")
}

#' @describeIn activity_frequency Computes the activity frequency for an \code{\link[bupaR]{activitylog}}.
#' @export
activity_frequency.activitylog <- function(log,
										   level = c("log", "trace", "activity", "case"),
										   sort = TRUE) {

	level <- rlang::arg_match(level)

	activity_frequency.eventlog(bupaR::to_eventlog(log), level = level, sort = sort)
}

#' @describeIn activity_frequency Computes the activity frequency for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
activity_frequency.grouped_activitylog <- function(log,
												   level = c("log", "trace", "activity", "case"),
												   sort = TRUE) {

	level <- rlang::arg_match(level)

	activity_frequency.grouped_eventlog(bupaR::to_eventlog(log), level = level, sort = sort)
}
