#' @title Resource Frequency
#'
#' @description Analyses the frequency of resources at different levels of analysis.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, summary statistics show the number of times a resource executes an activity in the complete log.
#' \item On \code{"case"} level, summary statistics of the frequency of resources can be used to get a better view on the
#' variance between the different cases, to get an insight into the number of different resources working on each case together
#' with the number of activities a resource executes per case.
#' \item On \code{"activity"} level, the resource frequency states how many different resources are executing a specific
#' activity in the complete log.
#' \item On \code{"resource"} level, this metric simply shows the absolute and relative frequency of occurrences of each
#' resource in the complete log.
#' \item On \code{"resource-activity"} level, the absolute and relative number of times each resource-activity combination
#' occurs in the complete log can be calculated. Two different relative numbers are provided here, one from the resource
#' perspective and one from the activity perspective. At the resource perspective, the denominator is the total number of
#' executions by the resource under consideration. At the activity perspective, the denominator is the total number of
#' occurrences of the activity under consideration.
#' }
#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#'
#' @family metrics
#'
#' @export resource_frequency
resource_frequency <- function(log,
							   level = c("log", "case", "activity", "resource", "resource-activity"),
							   append = deprecated(),
							   append_column = NULL,
							   sort = TRUE,
							   eventlog = deprecated()) {
	UseMethod("resource_frequency")
}

#' @describeIn resource_frequency Computes the resource frequency for an \code{\link[bupaR]{eventlog}}.
#' @export
resource_frequency.eventlog <- function(log,
										level = c("log", "case", "activity", "resource", "resource-activity"),
										append = deprecated(),
										append_column = NULL,
										sort = TRUE,
										eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "resource_frequency(eventlog)",
			with = "resource_frequency(log)")
		log <- eventlog
	}
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   level == "activity"~"median",
								   TRUE ~ "NA")
	}

	FUN <- switch(level,
				  log = resource_frequency_log,
				  case = resource_frequency_case,
				  activity = resource_frequency_activity,
				  resource = resource_frequency_resource,
				  "resource-activity" = resource_frequency_resource_activity)

	output <- FUN(log = log)

	if(level %in% c("resource", "resource-activity") && sort) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "resource_frequency", ifelse(level == "resource", 2,
																				ifelse(level == "resource-activity", 3,9)))
}

#' @describeIn resource_frequency Computes the resource frequency for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
resource_frequency.grouped_eventlog <- function(log,
												level = c("log", "case", "activity", "resource", "resource-activity"),
												append = deprecated(),
												append_column = NULL,
												sort = TRUE,
												eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   level == "activity"~"median",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = resource_frequency_log,
				  case = resource_frequency_case,
				  activity = resource_frequency_activity,
				  resource = resource_frequency_resource,
				  "resource-activity" = resource_frequency_resource_activity)

	output <- bupaR:::apply_grouped_fun(log, FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	#if(!(level %in% c("log"))) {
	#	grouped_metric(eventlog, FUN) -> output
	#}
	#else {
	#	grouped_metric_raw_log(eventlog, FUN)  -> output
	#}

	if(level %in% c("resource", "resource-activity") && sort) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "resource_frequency", ifelse(level == "resource", 2,
																				ifelse(level == "resource-activity", 3,9)))
}

#' @describeIn resource_frequency Computes the resource frequency for an \code{\link[bupaR]{activitylog}}.
#' @export
resource_frequency.activitylog <- function(log,
										   level = c("log", "case", "activity", "resource", "resource-activity"),
										   append = deprecated(),
										   append_column = NULL,
										   sort = TRUE,
										   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	resource_frequency.eventlog(bupaR::to_eventlog(log),
								level = level,
								append = append,
								append_column = append_column,
								sort = sort)
}

#' @describeIn resource_frequency Computes the resource frequency for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
resource_frequency.grouped_activitylog <- function(log,
												   level = c("log", "case", "activity", "resource", "resource-activity"),
												   append = deprecated(),
												   append_column = NULL,
												   sort = TRUE,
												   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	resource_frequency.grouped_eventlog(bupaR::to_eventlog(log),
										level = level,
										append = append,
										append_column = append_column,
										sort = sort)
}
