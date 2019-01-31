#' Metric: Activity Frequency
#'
#'
#' Provides summary statistics about the frequency of activity types at the level of log, traces, cases, activity types.
#'
#'
#' \itemize{ \item At log level, This metric shows the summary statistics of the frequency of activities throughout the complete event log.
#' \item  On the level of the cases, this metric showsthe absolute and relative number of times the different activity types occur in
#' each case. The absolute number shows the number of distinct activity types
#' that occur in each of the cases. The relative number is calculated based on the total activity executions in the case. \item On trace level, this metric
#' presents the absolute and relative number of times a specific activity type occurs in each trace. \item On the level of the activities, this metric
#' provides the absolute and relative frequency of a specific activity in the complete event log.
#' }
#'
#'
#' @param eventlog The dataset to be used. Should be a (grouped) eventlog object.
#' \code{eventlog}.
#'
#' @param level Level of granularity for the analysis: log, trace, case, activity. For more information, see \code{vignette("metrics", "edeaR")}
#' @param append Logical, indicating whether to append results to original event log. Ignored when level is log or trace.
#' @param append_column Which of the output columns to append to log, if append = T. Default column depends on chosen level.
#' @param sort Sort output on count. Defaults to TRUE. Only for levels with frequency count output.
#' @param ... Deprecated arguments
#'
#' @references Swennen, M. (2018). Using Event Log Knowledge to Support Operational Exellence Techniques (Doctoral dissertation). Hasselt University.
#'
#' @export activity_frequency

activity_frequency <- function(eventlog, level,  append, append_column, ...) {
	UseMethod("activity_frequency")
}


#' @describeIn activity_frequency Compute activity frequency for eventlog
#' @export

activity_frequency.eventlog <- function(eventlog,
										level = c("log","trace","activity","case"),
										append = F,
										append_column = NULL,
										sort = TRUE,
										...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

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


	output <- FUN(eventlog = eventlog)
	if(sort && level %in% c("case", "trace","activity")) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append, append_column, "activity_frequency")

}


#' @describeIn activity_frequency Compute activity frequency for grouped event log
#' @export

activity_frequency.grouped_eventlog <- function(eventlog,
												level = c("log","trace","activity","case"),
												append = F,
												append_column = NULL,
												sort = TRUE,
												...) {
	absolute <- NULL

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

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

	if(level != "log") {
		grouped_metric(eventlog, FUN) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN) -> output
	}
	if(sort && level %in% c("case", "trace","activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(eventlog, output, level, append, append_column, "activity_frequency")


}
