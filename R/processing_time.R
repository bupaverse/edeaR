#'  Metric: Processing time
#'
#'  Provides summary statistics about the processing time of the process.
#'
#'  In contrast to the throughput time of the cases in an event log, the metrics concerning
#'  the active time or the actual processing time provide summary statistics on the
#'  processing time of events on the level of the complete event log, the specific cases,
#'   traces, the activities, and the resource-activity combinations.
#'
#'   \itemize{
#'   \item On log level, this metric calculates the summary statistics of the actual processing time per case,
#'   summarised over the complete event log.
#'   \item On case level, a list of cases with their processing time are provided.
#'   \item On trace level, the summary statistics of processing time
#'   can be calculated for each possible sequence of activities that
#'   appears in the event log.
#'   \item Duration can also be calculated on the level
#'   of each activity. For each activity, an overview of the average processing time
#'   -or the service time- of this activity can be of interest.
#'   \item We can also look at the processing time per case
#'   on the level of each separate resource. This way, a company gets an overview of
#'   the amount of time each resource spends on a case and which resources spend
#'   more time on cases than others.
#'   \item On the resource-activity level, finally,
#'   we can have a look at the efficiency of resources by looking at the combination
#'   of each resource with each activity.
#'   }
#'
#' @param level Level of granularity for the analysis: log,  case, trace, activity, resource or resource-activity.
#' For more information, see \code{vignette("metrics", "edeaR")}
#'
#' @param sort Sort on decreasing processing time. For case level only.
#' @param work_schedule A schedule of working hours. If provided, only working hours are counted as processing time.
#'
#' @inherit activity_frequency params references seealso return
#' @inherit idle_time params
#'
#' @export processing_time

processing_time <- function(eventlog, level, append,append_column, units, sort, work_schedule, ...) {
	UseMethod("processing_time")
}

#' @describeIn processing_time Compute processing time for event log
#' @export

processing_time.eventlog <- function(eventlog,
									 level = c("log","trace","case","activity","resource","resource-activity"),
									 append = F,
									 append_column = NULL,
									 units = c("days","hours","mins","secs","weeks"),
									 sort = TRUE,
									 work_schedule = NULL,
									 ...){



	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)

	if(!is.null(work_schedule)){
		if(!("work_schedule" %in% class(work_schedule))) {
			stop("Make sure the work_schedule is created with the create_work_schedule function.")
		}
	}

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "processing_time",
								   level == "resource" ~ "median",
								   level == "resource-activity"~"median",
								   level == "activity"~"median",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = processing_time_log,
				  case = processing_time_case,
				  trace = processing_time_trace,
				  activity = processing_time_activity,
				  resource = processing_time_resource,
				  "resource-activity" = processing_time_resource_activity)

	output <- FUN(eventlog = eventlog, units = units, work_schedule = work_schedule)

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-processing_time) -> output
	}

	return_metric(eventlog, output, level, append, append_column,  "processing_time",
				  ifelse(level == "case", 1, 9),
				  empty_label = ifelse(level == "case",T, F)) -> t

	attr(t, "units") <- units
	t

}


#' @describeIn processing_time Compute processing time on grouped eventlog
#' @export

processing_time.grouped_eventlog <- function(eventlog,
											 level = c("log","trace","case","activity","resource","resource-activity"),
											 append = F,
											 append_column = NULL,
											 units = c("days","hours","mins","secs","weeks"),
											 sort = TRUE,
											 work_schedule = NULL,
											 ...){

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	units <- match.arg(units)

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
								   T ~ "NA")
	}

	if(!(level %in% c("log","activity","resource-activity","resource"))) {
		output <- grouped_metric(eventlog, FUN, units, work_schedule)
	}
	else {
		output <- grouped_metric_raw_log(eventlog, FUN, units, work_schedule)
	}

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-processing_time) -> output
	}


	return_metric(eventlog, output, level, append, append_column,  "processing_time",
				  ifelse(level == "case", 1, 9),
				  empty_label = ifelse(level == "case",T, F)) -> t

	attr(t, "units") <- units
	t

}
