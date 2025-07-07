#' @title Processing Time
#'
#' @description Provides summary statistics about the processing time of the process.
#'
#' In contrast to the [`throughput_time()`] of the cases in a log, the metrics concerning the active time
#' or the actual processing time provide summary statistics on the processing time of events on the level of the complete log,
#' the specific cases, traces, the activities, and the resource-activity combinations.
#'
#' @param level [`character`] (default `"log"`): Level of granularity for the analysis: `"log"` (default), `"trace"`, `"case"`,
#' `"activity"`, `"resource"`, or `"resource-activity"`. For more information, see `vignette("metrics", "edeaR")` and **Details** below.
#' @param units [`character`] (default `"auto"`): The time unit in which the processing times should be reported. Should be one of the following values:
#' `"auto"` (default), `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the `units` argument of [`difftime()`].
#' @param sort [`logical`] (default `TRUE`): Sort on decreasing processing time. For `"case"` `level` only.
# TODO: update work_schedule arg
#' @param work_schedule A schedule of working hours. If provided, only working hours are counted as processing time.
#'
#' @details
#' Argument `level` has the following options:
#' * At `"log"` level, this metric calculates the summary statistics of the actual processing time per case,
#' summarised over the complete event log.
#' * On `"trace"` level, the summary statistics of processing time can be calculated for each possible sequence of activities
#' that appears in the event log.
#' * On `"case"` level, a list of cases with their processing time are provided.
#' * On `"activity"` level, an overview of the average processing time -or the service time- of each activity can be calculated.
#' * At `"resource"` level, this metric calculates the processing time per resource.
#' * On `"resource-activity"` level, the efficiency of resources by looking at the combination of each resource
#' with each activity can be investigated.
#'
#' @inherit activity_frequency params references seealso return
#'
#' @seealso [`idle_time()`],[`throughput_time()`],[`difftime()`]
#'
#' @family metrics
#'
#' @concept metrics_time
#'
#' @export processing_time
processing_time <- function(log,
							level = c("log", "trace", "case", "activity", "resource", "resource-activity","activity-instance"),
							append = deprecated(),
							append_column = NULL,
							units = c("auto", "secs", "mins", "hours", "days", "weeks"),
							sort = TRUE,
							work_schedule = NULL,
							eventlog = deprecated()) {
	UseMethod("processing_time")
}

#' @describeIn processing_time Computes processing time for an [`eventlog`][`bupaR::eventlog`].
#' @export
processing_time.eventlog <- function(log,
									 level = c("log", "trace", "case", "activity", "resource", "resource-activity","activity-instance"),
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
				  "resource-activity" = processing_time_resource_activity,
				  "activity-instance" = processing_time_activity_instance)

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

	return(t)
}

#' @describeIn processing_time Computes processing time for a [`grouped_eventlog`][`bupaR::grouped_eventlog`].
#' @export
processing_time.grouped_eventlog <- function(log,
											 level = c("log", "trace", "case", "activity", "resource", "resource-activity", "activity-instance"),
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
				  "resource-activity" = processing_time_resource_activity,
				  "activity-instance" = processing_time_activity_instance)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "processing_time",
								   level == "resource" ~ "median",
								   level == "resource-activity"~"median",
								   level == "activity"~"median",
								   TRUE ~ "NA")
	}

	output <- bupaR:::apply_grouped_fun(log, fun = FUN, units, work_schedule, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	if(level %in% c("log", "trace", "activity", "resource", "resource-activity")) {
		time_units <- attr(output$min, "units")
	} else {
		time_units <- attr(output$processing_time, "units")
	}

	#if(!(level %in% c("log","activity","resource-activity","resource"))) {
	#	output <- grouped_metric(log, FUN, units, work_schedule)
	#}
	#else {
	#	output <- grouped_metric_raw_log(log, FUN, units, work_schedule)
	#}

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-processing_time) -> output
	}


	return_metric(log, output, level, append, append_column,  "processing_time",
				  ifelse(level == "case", 1, 9),
				  empty_label = ifelse(level == "case",TRUE, FALSE)) -> t

	attr(t, "units") <- time_units

	return(t)
}

#' @describeIn processing_time Computes processing time for an [`activitylog`][`bupaR::activitylog`].
#' @export
processing_time.activitylog <- function(log,
										level = c("log", "trace", "case", "activity", "resource", "resource-activity", "activity-instance"),
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

#' @describeIn processing_time Computes processing time for a [`grouped_activitylog`][`bupaR::grouped_activitylog`].
#' @export
processing_time.grouped_activitylog <- function(log,
												level = c("log", "trace", "case", "activity", "resource", "resource-activity", "activity-instance"),
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
