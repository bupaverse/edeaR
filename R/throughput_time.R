#' @title Throughput Time of Cases
#'
#' @description Provides summary statistics concerning the throughput times of cases.
#'
#' @param level [`character`] (default `"log"`): Level of granularity for the analysis: `"log"` (default),
#' `"trace"`, or `"case"`. For more information, see `vignette("metrics", "edeaR")` and **Details** below.
#' @param units [`character`] (default `"auto"`): The time unit in which the throughput times should be reported.
#' Should be one of the following values: `"auto"` (default), `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`.
#' See also the `units` argument of [`difftime`].
#'
#' @details
#' Argument `level` has the following options:
#' * At `"log"` level, the summary statistics describing the throughput time of cases in an aggregated fashion.
#' * On `"trace"` level, the throughput time of the different process variants or traces in the log are calculated.
#' * On `"case"` level, the throughput time is defined as the total duration of the case, or the difference between
#' the timestamp of the end event and the timestamp of the start event of the case. Possible [`idle_time()`] is also included
#' in this calculation.
#' * On `"activity-instance"` level, the throughput time of each activity instance. Throughput here is defined as the difference between the first and last event, without considering the lifecycle status. For the lifecycle-aware throughput time (e.g. not incorporating the time the activity is "suspended"), see processing time.
#' * on `'activity` level, summary statistics describing the throuhgput time of activity instances (see above) per activity type.
#' For other levels (e.g. `"activity"`, `"resource"`, or `"resource-activity"`), the throughput time is equal
#' to the [`processing_time()`] and are, therefore, not supported by this method.
#'
#' @inherit activity_frequency params references seealso return
#' @inherit processing_time params
#'
#' @seealso [`idle_time()`],[`processing_time()`],[`difftime()`]
#'
#' @family metrics
#'
#' @concept metrics_time
#'
#' @export throughput_time
throughput_time <- function(log,
							level = c("log", "trace", "case", "activity","activity-instance"),
							append = deprecated(),
							append_column = NULL,
							units = c("auto", "secs", "mins", "hours", "days", "weeks"),
							sort = TRUE,
							work_schedule = NULL,
							eventlog = deprecated()) {
	UseMethod("throughput_time")
}

#' @describeIn throughput_time Computes throughput time for an [`eventlog`][`bupaR::eventlog`].
#' @export
throughput_time.eventlog <- function(log,
									 level = c("log", "trace", "case", "activity","activity-instance"),
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

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "throughput_time",
								   TRUE ~ "NA")
	}

	FUN <- switch(level,
				  log = throughput_time_log,
				  case = throughput_time_case,
				  trace = throughput_time_trace,
				  "activity-instance" = throughput_time_activity_instance,
				  activity = throughput_time_activity)

	output <- FUN(log = log, units = units, work_schedule = work_schedule)

	time_units <- attr(output, "units")

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-throughput_time) -> output
	}

	output <- return_metric(log, output, level, append, append_column,  "throughput_time", 1, empty_label = TRUE)

	# TODO: set units according to difftime units from output (useful in case the user set units = "auto")
	attr(output, "units") <- time_units
	return(output)
}

#' @describeIn throughput_time Computes throughput time for a [`grouped_eventlog`][`bupaR::grouped_eventlog`].
#' @export
throughput_time.grouped_eventlog <- function(log,
											 level = c("log", "trace", "case", "activity","activity-instance"),
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

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "throughput_time",
								   TRUE ~ "NA")
	}

	FUN <- switch(level,
				  log = throughput_time_log,
				  case = throughput_time_case,
				  trace = throughput_time_trace)

	output <- bupaR:::apply_grouped_fun(log, fun = FUN, units, work_schedule, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	if(level %in% c("log", "trace")) {
		time_units <- attr(output$min, "units")
	} else {
		time_units <- attr(output$throughput_time, "units")
	}

	#if(level != "log") {
	#	grouped_metric(log, FUN, units, work_schedule) -> output
	#}
	#else {
	#	grouped_metric_raw_log(log, FUN, units, work_schedule) -> output
	#}

	if(sort && level %in% c("case")) {
		output %>%
			arrange(-throughput_time) -> output
	}

	output <- return_metric(log, output, level, append, append_column, "throughput_time", 1, empty_label = TRUE)

	attr(output, "units") <- time_units

	return(output)
}

#' @describeIn throughput_time Computes throughput time for an [`activitylog`][`bupaR::activitylog`].
#' @export
throughput_time.activitylog <- function(log,
										level = c("log", "trace", "case", "activity","activity-instance"),
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

	throughput_time.eventlog(bupaR::to_eventlog(log),
							 level = level,
							 append = append,
							 append_column = append_column,
							 units = units,
							 sort = sort,
							 work_schedule = work_schedule)
}

#' @describeIn throughput_time Computes throughput time for a [`grouped_activitylog`][`bupaR::grouped_activitylog`].
#' @export
throughput_time.grouped_activitylog <- function(log,
												level = c("log", "trace", "case", "activity","activity-instance"),
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

	throughput_time.grouped_eventlog(bupaR::to_eventlog(log),
									 level = level,
									 append = append,
									 append_column = append_column,
									 units = units,
									 sort = sort,
									 work_schedule = work_schedule)
}
