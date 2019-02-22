#' Metric: Throughput time of cases
#'
#'
#' Provides summary statistics concerning the throughput times of cases.
#'
#'
#'
#' \itemize{
#'
#' \item The throughput time of a case is the total duration
#' of the case, or the difference between the timestamp of the end event and the
#' timestamp of the start event of the case. Possible idle time is also included
#' in this calculation.
#' \item On log level, the summary statistics of these throughput to describe the throughput time of cases in an
#' aggregated fashion.
#' \item Instead of looking at all cases in the log, it
#' can be interesting to analyse the different process variants or traces in the log
#' }
#'
#'
#'
#' @param level Level of granularity for the analysis: log,  case, activity, resource or resource-activity.
#' For more information, see \code{vignette("metrics", "edeaR")}
#' @param sort Sort by decreasing throughput time. Defaults to true. Only relevant for case level.

#'
#' @inherit activity_frequency params references seealso return
#' @inherit idle_time params
#'
#'
#'
#' @export throughput_time
#'


throughput_time <- function(eventlog, level, append, append_column, units, ...) {
	UseMethod("throughput_time")
}

#' @describeIn throughput_time Throughput time for eventlog
#' @export


throughput_time.eventlog <- function(eventlog,
									 level = c("log","trace","case"),
									 append = FALSE,
									 append_column = NULL,
									 units = c("days","hours","mins","secs","weeks"),
									 sort = TRUE,
									 ...){

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "throughput_time",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = throughput_time_log,
				  case = throughput_time_case,
				  trace = throughput_time_trace)


	output <- FUN(eventlog = eventlog, units = units)
	if(sort && level %in% c("case")) {
		output %>%
			arrange(-throughput_time) -> output
	}
	output <- return_metric(eventlog, output, level, append, append_column,  "throughput_time", 1, empty_label = T)
	attr(output, "units") <- units

	return(output)
}


#' @describeIn throughput_time Throughput time for grouped eventlog
#' @export

throughput_time.grouped_eventlog <- function(eventlog,
											 level = c("log","trace","case"),
											 append = FALSE,
											 append_column = NULL,
											 units = c("days","hours","mins","secs","weeks"),
											 sort = TRUE,
											 ...){

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)


	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "throughput_time",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = throughput_time_log,
				  case = throughput_time_case,
				  trace = throughput_time_trace)


	if(level != "log") {
		grouped_metric(eventlog, FUN, units) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN, units) -> output
	}

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-throughput_time) -> output
	}

	output <- return_metric(eventlog, output, level, append, append_column, "throughput_time", 1, empty_label = T)


	attr(output, "units") <- units

	return(output)
}
