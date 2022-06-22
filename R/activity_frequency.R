#' @title Metric: Activity Frequency
#'
#' @description Provides summary statistics about the frequency of activity types at the level of log, traces, cases, activity types.
#'
#' @param log \code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}}, \code{\link[bupaR]{grouped_log}}, \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"}, \code{"trace"}, \code{"case"}, or \code{"activity"}.
#' For more information, see \code{vignette("metrics", "edeaR")} and 'Details' below.
#' @param append \code{\link{logical}} (default \code{FALSE}) `r lifecycle::badge("deprecated")`: Indicating whether to append results to original log. Ignored when level is \code{log} or \code{trace}.
#' @param append_column `r lifecycle::badge("deprecated")` Which of the output columns to append to log, if \code{append = TRUE}. Default column depends on chosen level.
#' @param sort \code{\link{logical}} (default \code{TRUE}): Sort output on count. Only for levels with frequency count output.
#' @param ... `r lifecycle::badge("deprecated")` Deprecated arguments
#' @param eventlog `r lifecycle::badge("deprecated")`; please use \code{log} instead.
#'
#' @details
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
#' @references Swennen, M. (2018). Using Event Log Knowledge to Support Operational Exellence Techniques (Doctoral dissertation). Hasselt University.
#'
#' @export activity_frequency
activity_frequency <- function(log, level = c("log", "trace", "activity", "case"), append = deprecated(), append_column = NULL, sort = TRUE, ..., eventlog = deprecated()) {
	UseMethod("activity_frequency")
}


#' @describeIn activity_frequency Compute activity frequency for an \code{\link[bupaR]{eventlog}}.
#' @export
activity_frequency.eventlog <- function(log, level = c("log", "trace", "activity", "case"), append = deprecated(), append_column = NULL, sort = TRUE, ..., eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	if(is.null(append_column)) {
		append_column <- case_when(level == "activity" ~ "absolute",
								   level == "case" ~ "absolute",
								   T ~ "NA")
	}

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

	return_metric(log, output, level, append, append_column, "activity_frequency")
}

#' @describeIn activity_frequency Compute activity frequency for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
activity_frequency.grouped_eventlog <- function(log, level = c("log", "trace", "activity", "case"), append = deprecated(), append_column = NULL, sort = TRUE, ..., eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	if(is.null(append_column)) {
		append_column <- case_when(level == "activity" ~ "absolute",
								   level == "case" ~ "absolute",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = activity_frequency_log,
				  case = activity_frequency_case,
				  trace = activity_frequency_trace,
				  activity = activity_frequency_activity)

	# Doesn't work!
	bupaR:::apply_grouped_fun(log, FUN, .keep_groups = TRUE, .returns_log = TRUE) -> output

	#if(level != "log") {
	#	grouped_metric(log, FUN) -> output
	#} else {
	#	grouped_metric_raw_log(log, FUN) -> output
	#}

	if(sort && level %in% c("case", "trace","activity")) {
		output %>%
			arrange(-.data[["absolute"]]) -> output
	}

	return_metric(log, output, level, append, append_column, "activity_frequency")
}

#' @describeIn activity_frequency Compute activity frequency for an \code{\link[bupaR]{activitylog}}.
#' @export
activity_frequency.activitylog <- function(log, level = c("log", "trace", "activity", "case"), append = deprecated(), append_column = NULL, sort = TRUE, ..., eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	activity_frequency.eventlog(bupaR::to_eventlog(log), level = level, append = append, append_column = append_column, sort = sort, ...)
}

#' @describeIn activity_frequency Compute activity frequency for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
activity_frequency.grouped_activitylog <- function(log, level = c("log", "trace", "activity", "case"), append = deprecated(), append_column = NULL, sort = TRUE, ..., eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	activity_frequency.grouped_eventlog(bupaR::to_eventlog(log), level = level, append = append, append_column = append_column, sort = sort, ...)
}