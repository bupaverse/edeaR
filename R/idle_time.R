#' @title Idle Time
#'
#' @description Calculates the amount of time that no activity occurs.
#'
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"} (default),
#' \code{"trace"}, \code{"case"}, or \code{"resource"}. For more information, see \code{vignette("metrics", "edeaR")} and 'Details' below.
#' @param units \code{\link{character}} (default \code{"auto"}): The time unit in which the throughput times should be reported. Should be one of the following values:
#' \code{"auto"} (default), \code{"secs"}, \code{"mins"}, \code{"hours"}, \code{"days"}, \code{"weeks"}. See also the \code{units} argument of \code{\link{difftime}}.
#' @param sort \code{\link{logical}} (default \code{TRUE}): Sort by decreasing idle time. Only relevant for \code{"trace"} and \code{"resource"} \code{level}.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item  At \code{"log"} level, the idle time metric provides an overview of summary statistics of the idle time per case,
#' aggregated over the complete log.
#' \item On \code{"trace"} level, the idle time metric provides an overview of the summary statistics of the idle time for each trace in the log.
#' \item On \code{"case"} level, the idle time metric provides an overview of the total idle time per case
#' \item On \code{"resource"} level, this metric can be used to get an insight in the amount of time each resource "wastes" during the process.
#' }
#'
#' @inherit activity_frequency params references seealso return
#'
#' @family metrics
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

#' @describeIn idle_time Computes the idle time for an \code{\link[bupaR]{eventlog}}.
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
	t
}

#' @describeIn idle_time Computes the idle time for a \code{\link[bupaR]{grouped_eventlog}}.
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

	#if(level != "log") {
	#	grouped_metric(eventlog, FUN, units) -> output
	#}
	#else {
	#	grouped_metric_raw_log(eventlog, FUN, units) -> output
	#}

	if(sort && level %in% c("case","resource")) {
		output %>%
			arrange(-idle_time) -> output
	}

	return_metric(log, output, level, append, append_column, "idle_time", 1, empty_label = TRUE) -> t

	# TODO: set units according to difftime units from output (useful in case the user set units = "auto")
	attr(t, "units") <- units
	t
}

#' @describeIn idle_time Computes the idle time for an \code{\link[bupaR]{activitylog}}.
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

#' @describeIn idle_time Computes the idle time for a \code{\link[bupaR]{grouped_activitylog}}.
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