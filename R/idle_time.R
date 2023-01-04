#' @title Idle Time
#'
#' @description Calculates the amount of time that no activity occurs.
#'
#' @param level [`character`] (default `"log"`): Level of granularity for the analysis: `"log"` (default),
#' `"trace"`, `"case"`, or `"resource"`. For more information, see `vignette("metrics", "edeaR")` and **Details** below.
#' @param units [`character`] (default `"auto"`): The time unit in which the throughput times should be reported. Should be one of the following values:
#' `"auto"` (default), `"secs"`, `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the `units` argument of [`difftime()`].
#' @param sort [`logical`] (default `TRUE`): Sort by decreasing idle time. Only relevant for `"trace"` and `"resource"` `level`.
#'
#' @details
#' Argument `level` has the following options:
#' * At `"log"` level, the idle time metric provides an overview of summary statistics of the idle time per case,
#' aggregated over the complete log.
#' * On `"trace"` level, the idle time metric provides an overview of the summary statistics of the idle time for each trace in the log.
#' * On `"case"` level, the idle time metric provides an overview of the total idle time per case
#' * On `"resource"` level, this metric can be used to get an insight in the amount of time each resource "wastes" during the process.
#'
#' @inherit activity_frequency params references seealso return
#'
#' @seealso [`throughput_time()`],[`processing_time()`],[`difftime()`]
#'
#' @family metrics
#'
#' @concept metrics_time
#'
#' @export idle_time
idle_time <- function(log,
					  level = c("log", "trace", "case", "resource"),
					  append = deprecated(),
					  append_column = NULL,
					  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
					  sort = TRUE,
					  eventlog = deprecated()) {
	UseMethod("idle_time")
}

#' @describeIn idle_time Computes the idle time for an [`eventlog`][`bupaR::eventlog`].
#' @export
idle_time.eventlog <- function(log,
							   level = c("log", "trace", "case", "resource"),
							   append = deprecated(),
							   append_column = NULL,
							   units = c("auto", "secs", "mins", "hours", "days", "weeks"),
							   sort = TRUE,
							   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "idle_time",
								   level == "resource" ~ "idle_time",
								   TRUE ~ "NA")
	}

	FUN <- switch(level,
				  log = idle_time_log,
				  case = idle_time_case,
				  trace = idle_time_trace,
				  resource = idle_time_resource)

	output <- FUN(log = log, units = units)

	time_units <- attr(output, "units")

	if(sort && level %in% c("case","resource")) {
		output %>%
			arrange(-idle_time) -> output
	}

	return_metric(log, output, level, append, append_column, "idle_time", 1, empty_label = TRUE) -> t

	attr(t, "units") <- time_units

	return(t)
}

#' @describeIn idle_time Computes the idle time for a [`grouped_eventlog`][`bupaR::grouped_eventlog`].
#' @export
idle_time.grouped_eventlog <- function(log,
									   level = c("log", "case", "trace", "resource"),
									   append = deprecated(),
									   append_column = NULL,
									   units = c("auto", "secs", "mins", "hours", "days", "weeks"),
									   sort = TRUE,
									   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "idle_time",
								   level == "resource" ~ "idle_time",
								   TRUE ~ "NA")
	}

	FUN <- switch(level,
				  log = idle_time_log,
				  case = idle_time_case,
				  trace = idle_time_trace,
				  resource = idle_time_resource)

	output <- bupaR:::apply_grouped_fun(log, fun = FUN, units, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	if(level %in% c("log", "trace")) {
		time_units <- attr(output$min, "units")
	} else {
		time_units <- attr(output$idle_time, "units")
	}

	if(sort && level %in% c("case","resource")) {
		output %>%
			arrange(-idle_time) -> output
	}

	return_metric(log, output, level, append, append_column, "idle_time", 1, empty_label = TRUE) -> t

	attr(t, "units") <- time_units

	return(t)
}

#' @describeIn idle_time Computes the idle time for an [`activitylog`][`bupaR::activitylog`].
#' @export
idle_time.activitylog <- function(log,
								  level = c("log", "trace", "case", "resource"),
								  append = deprecated(),
								  append_column = NULL,
								  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
								  sort = TRUE,
								  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	idle_time.eventlog(bupaR::to_eventlog(log),
					   level = level,
					   append = append,
					   append_column = append_column,
					   units = units,
					   sort = sort)
}

#' @describeIn idle_time Computes the idle time for a [`grouped_activitylog`][`bupaR::grouped_activitylog`].
#' @export
idle_time.grouped_activitylog <- function(log,
										  level = c("log", "trace", "case", "resource"),
										  append = deprecated(),
										  append_column = NULL,
										  units = c("auto", "secs", "mins", "hours", "days", "weeks"),
										  sort = TRUE,
										  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)
	units <- rlang::arg_match(units)

	idle_time.grouped_eventlog(bupaR::to_eventlog(log),
							   level = level,
							   append = append,
							   append_column = append_column,
							   units = units,
							   sort = sort)
}