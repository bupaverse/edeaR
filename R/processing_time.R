#' @title Processing Time
#'
#' @description Provides summary statistics about the processing time of the process.
#'
#' In contrast to the \code{\link{throughput_time}} of the cases in a log, the metrics concerning the active time
#' or the actual processing time provide summary statistics on the processing time of events on the level of the complete log,
#' the specific cases, traces, the activities, and the resource-activity combinations.
#'
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"} (default),
#' \code{"trace"}, \code{"case"}, \code{"activity"}, \code{"resource"}, or \code{"resource-activity"}. For more information,
#' see \code{vignette("metrics", "edeaR")} and 'Details' below.
#' @param units \code{\link{character}} (default \code{"auto"}): The time unit in which the processing times should be reported. Should be one of the following values:
#' \code{"auto"} (default), \code{"secs"}, \code{"mins"}, \code{"hours"}, \code{"days"}, \code{"weeks"}. See also the \code{units} argument of \code{\link{difftime}}.
#' @param sort \code{\link{logical}} (default \code{TRUE}): Sort on decreasing processing time. For \code{"case"} \code{level} only.
# TODO: update work_schedule arg
#' @param work_schedule A schedule of working hours. If provided, only working hours are counted as processing time.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, this metric calculates the summary statistics of the actual processing time per case,
#' summarised over the complete event log.
#' \item On \code{"trace"} level, the summary statistics of processing time can be calculated for each possible sequence of activities
#' that appears in the event log.
#' \item On \code{"case"} level, a list of cases with their processing time are provided.
#' \item On \code{"activity"} level, an overview of the average processing time -or the service time- of each activity
#' can be calculated.
#' \item At \code{"resource"} level, this metric calculates the processing time per resource.
#' \item On \code{"resource-activity"} level, the efficiency of resources by looking at the combination of each resource
#' with each activity can be investigated.
#' }
#'
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link{throughput_time}},\code{\link{difftime}}
#'
#' @family metrics
#'
#' @export processing_time
processing_time <- function(log,
							level = c("log", "trace", "case", "activity", "resource", "resource-activity"),
							append = deprecated(),
							append_column = NULL,
							units = c("auto", "secs", "mins", "hours", "days", "weeks"),
							sort = TRUE,
							work_schedule = NULL,
							eventlog = deprecated()) {
	UseMethod("processing_time")
}

#' @describeIn processing_time Computes processing time for an \code{\link[bupaR]{eventlog}}.
#' @export
processing_time.eventlog <- function(log,
									 level = c("log", "trace", "case", "activity", "resource", "resource-activity"),
									 append = deprecated(),
									 append_column = NULL,
									 units = c("auto", "secs", "mins", "hours", "days", "weeks"),
									 sort = TRUE,
									 work_schedule = NULL,
									 eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	if(!is.null(work_schedule)) {
		if(!("work_schedule" %in% class(work_schedule))) {
			stop("Make sure the work_schedule is created with the create_work_schedule function.")
		}
	}

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "processing_time",
								   level == "resource" ~ "median",
								   level == "resource-activity"~"median",
								   level == "activity"~"median",
								   TRUE ~ "NA")
	}

	FUN <- switch(level,
				  log = processing_time_log,
				  case = processing_time_case,
				  trace = processing_time_trace,
				  activity = processing_time_activity,
				  resource = processing_time_resource,
				  "resource-activity" = processing_time_resource_activity)

	output <- FUN(log = log, units = units, work_schedule = work_schedule)

	time_units <- attr(output, "units")

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-processing_time) -> output
	}

	return_metric(log, output, level, append, append_column,  "processing_time",
				  ifelse(level == "case", 1, 9),
				  empty_label = ifelse(level == "case",TRUE, FALSE)) -> t

	attr(t, "units") <- time_units
	t
}

#' @describeIn processing_time Computes processing time for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
processing_time.grouped_eventlog <- function(log,
											 level = c("log", "trace", "case", "activity", "resource", "resource-activity"),
											 append = deprecated(),
											 append_column = NULL,
											 units = c("auto", "secs", "mins", "hours", "days", "weeks"),
											 sort = TRUE,
											 work_schedule = NULL,
											 eventlog = deprecated()){

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	if(!is.null(work_schedule)){
		if(!("work_schedule" %in% class(work_schedule))) {
			stop("Make sure the work_schedule is created with the create_work_schedule function.")
		}
	}

	FUN <- switch(level,
				  log = processing_time_log,
				  case = processing_time_case,
				  trace = processing_time_trace,
				  activity = processing_time_activity,
				  resource = processing_time_resource,
				  "resource-activity" = processing_time_resource_activity)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "processing_time",
								   level == "resource" ~ "median",
								   level == "resource-activity"~"median",
								   level == "activity"~"median",
								   TRUE ~ "NA")
	}

	if(!(level %in% c("log","activity","resource-activity","resource"))) {
		output <- grouped_metric(log, FUN, units, work_schedule)
	}
	else {
		output <- grouped_metric_raw_log(log, FUN, units, work_schedule)
	}

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-processing_time) -> output
	}


	return_metric(log, output, level, append, append_column,  "processing_time",
				  ifelse(level == "case", 1, 9),
				  empty_label = ifelse(level == "case",TRUE, FALSE)) -> t

	attr(t, "units") <- units
	t

}

#' @describeIn processing_time Computes processing time for an \code{\link[bupaR]{activitylog}}.
#' @export
processing_time.activitylog <- function(log,
										level = c("log", "trace", "case", "activity", "resource", "resource-activity"),
										append = deprecated(),
										append_column = NULL,
										units = c("auto", "secs", "mins", "hours", "days", "weeks"),
										sort = TRUE,
										work_schedule = NULL,
										eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	processing_time.eventlog(bupaR::to_eventlog(log),
							 level = level,
							 append = append,
							 append_column = append_column,
							 units = units,
							 sort = sort,
							 work_schedule = work_schedule)
}

#' @describeIn processing_time Computes processing time for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
processing_time.grouped_activitylog <- function(log,
												level = c("log", "trace", "case", "activity", "resource", "resource-activity"),
												append = deprecated(),
												append_column = NULL,
												units = c("auto", "secs", "mins", "hours", "days", "weeks"),
												sort = TRUE,
												work_schedule = NULL,
												eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	processing_time.grouped_eventlog(bupaR::to_eventlog(log),
									 level = level,
									 append = append,
									 append_column = append_column,
									 units = units,
									 sort = sort,
									 work_schedule = work_schedule)
}
