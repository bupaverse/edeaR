#' @title Number of Repetitions
#'
#' @description Provides information statistics on the number of repetitions
#'
#' A repetition is an execution of an activity within a case while that activity has already been executed before, but
#' one or more other activities are executed in between.
#'
#' @param type \code{\link{character}} (default \code{"all"}): The type of repetitions: \code{"all"} (default),
#' \code{"repeat"}, or \code{"redo"}. For more information, see 'Details' below.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, this metric shows the summary statistics of the number of repetitions within a case,
#' which can provide insights in the amount of waste in a log. Each combination of two or more occurrences of the same activity,
#' executed not immediately following each other, by the same resource is counted as one repeat repetition of this activity.
#' \item On \code{"case"} level, this metric provides the absolute and relative number of repetitions in each case.
#' \item On \code{"activity"} level, this metric shows which activities occur the most in a repetition. The absolute and
#' relative number of both repeat and redo repetitions is provided by this metric, giving an overview per activity.
#' \item On \code{"resource"} level, it can be interesting to have an overview of which resources need more than one time
#' to execute an activity in a case or which resources need to have an activity redone later on in the case by another resource.
#' This metric provides the absolute and relative number of times each resource appears in a repetition.
#' \item On \code{"resource-activity"} level, this metric provides specific information about which activities and which
#' resources are involved in the repetitions. For this metric the absolute and relative number of repeat and redo repetitions
#' is provided. Again, two difierent relative numbers are provided, one relative to the total number of executions of
#' the activity in the complete log, and one relative to the total number of executions performed by the resource throughout
#' the complete log.
#' }
#'
#' Similar to the [self-loop][number_of_selfloops] metric, a distinction should be made between \code{"repeat"} and
#' \code{"redo"} repetitions, as can be set by the \code{type} argument:
#' \itemize{
#' \item \code{"repeat"} repetitions are activity executions of the same activity type that are executed not immediately
#' following each other, but by the same resource.
#' \item \code{"redo"} repetitions are activity executions of the same activity type that are executed not immediately
#' following each other and by a different resource than the first activity occurrence of this activity type.
#' }
#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link{number_of_selfloops}}
#'
#' @family metrics
#'
#' @concept metrics_repetition
#'
#' @export number_of_repetitions
number_of_repetitions <- function(log,
								  type = c("all", "repeat", "redo"),
								  level = c("log", "case", "activity", "resource", "resource-activity"),
								  sort = TRUE) {
	UseMethod("number_of_repetitions")
}

#' @describeIn number_of_repetitions Computes the number of repetitions for an \code{\link[bupaR]{eventlog}}.
#' @export
number_of_repetitions.eventlog <- function(log,
										   type = c("all", "repeat", "redo"),
										   level = c("log", "case", "activity", "resource", "resource-activity"),
										   sort = TRUE) {


	type <- rlang::arg_match(type)
	level <- rlang::arg_match(level)

	absolute <- NULL
	#if(all((type) == c("all", "repeat","redo")))
	#	message("Using default type: all")
	#if(all((level) == c("log","case","activity","resource","resource-activity")))
	#	message("Using default level: log")


	if(type == "all") {
		FUN <- switch(level,
					  log = all_repetitions_log,
					  case = all_repetitions_case,
					  activity = all_repetitions_activity,
					  resource = all_repetitions_resource,
					  "resource-activity" = repeat_repetitions_resource_activity
		)
	}
	else if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_log,
					  case = repeat_repetitions_case,
					  activity = repeat_repetitions_activity,
					  resource = repeat_repetitions_resource,
					  "resource-activity" = repeat_repetitions_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_repetitions_log,
					  case = redo_repetitions_case,
					  activity = redo_repetitions_activity,
					  resource = redo_repetitions_resource,
					  "resource-activity" = redo_repetitions_resource_activity
		)
	}

	output <- FUN(eventlog = log)

	if(sort && level %in% c("case", "resource", "activity", "resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric_v2(log, output, level, "number_of_repetitions")
	attr(output, "type") <- type

	return(output)
}

#' @describeIn number_of_repetitions Computes the number of repetitions for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
number_of_repetitions.grouped_eventlog <- function(log,
												   type = c("all", "repeat", "redo"),
												   level = c("log", "case", "activity", "resource", "resource-activity"),
												   sort = TRUE) {

	type <- rlang::arg_match(type)
	level <- rlang::arg_match(level)

	absolute <- NULL

	if(type == "all") {
		FUN <- switch(level,
					  log = all_repetitions_log,
					  case = all_repetitions_case,
					  activity = all_repetitions_activity,
					  resource = all_repetitions_resource,
					  "resource-activity" = repeat_repetitions_resource_activity
		)
	}
	else if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_log,
					  case = repeat_repetitions_case,
					  activity = repeat_repetitions_activity,
					  resource = repeat_repetitions_resource,
					  "resource-activity" = repeat_repetitions_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_repetitions_log,
					  case = redo_repetitions_case,
					  activity = redo_repetitions_activity,
					  resource = redo_repetitions_resource,
					  "resource-activity" = redo_repetitions_resource_activity
		)

	}

	output <- apply_grouped_fun(log, fun = FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	#if(level != "log") {
	#	grouped_metric(eventlog, FUN) -> output
	#}
	#else {
	#	grouped_metric_raw_log(eventlog, FUN) -> output
	#}


	if(sort && level %in% c("case", "resource", "activity", "resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric_v2(log, output, level, "number_of_repetitions")
	attr(output, "type") <- type

	return(output)
}

#' @describeIn number_of_repetitions Computes the number of repetitions for an \code{\link[bupaR]{activitylog}}.
#' @export
number_of_repetitions.activitylog <- function(log,
											  type = c("all", "repeat", "redo"),
											  level = c("log", "case", "activity", "resource", "resource-activity"),
											  sort = TRUE) {

	type <- rlang::arg_match(type)
	level <- rlang::arg_match(level)

	number_of_repetitions.eventlog(bupaR::to_eventlog(log),
								   type = type,
								   level = level,
								   sort = sort)
}

#' @describeIn number_of_repetitions Computes the number of repetitions for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
number_of_repetitions.grouped_activitylog <- function(log,
													  type = c("all", "repeat", "redo"),
													  level = c("log", "case", "activity", "resource", "resource-activity"),
													  sort = TRUE) {

	type <- rlang::arg_match(type)
	level <- rlang::arg_match(level)

	number_of_repetitions.grouped_eventlog(bupaR::to_eventlog(log),
										   type = type,
										   level = level,
										   sort = sort)
}
