#' @title Number of Self-loops
#'
#' @description Provides information statistics on the number of self-loops in a trace.
#'
#' Activity instances of the same activity type that are executed more than once immediately after each other by the same
#' resource are in a self-loop ("length-1-loop"). If an activity instance of the same activity type is executed 3 times
#' after each other by the same resource, this is defined as a "size 2 self-loop".
#'
#' @details
#' Two types of self-loops are defined, which can be chosen using the \code{type} argument:
#' \itemize{
#' \item \code{"repeat"} self-loops are activity executions of the same activity type that are executed immediately following
#' each other by the same resource.
#' \item \code{"redo"} self-loops are activity executions of the same activity type that are executed immediately following
#' each other by a different resource.
#' }
#'
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, the summary statistics of the number of self-loops within a trace can give a first insight
#' in the amount of waste in a log. As stated earlier, each combination of two occurrences of the same activity executed
#' by the same resource will be counted as one repeat self-loop of this activity.
#' \item On \code{"case"} level, an overview is provided of the absolute and relative number of repeat and redo self-loops
#' in each case. To calculate the relative number, each (repeat or redo) self-loop is counted as 1 occurrence, and the other
#' activity instances are also counted as 1.
#' \item On \code{"activity"} level, the absolute and relative number of self-loops per activity can be an indication for
#' which activities are causing the most waste in the process.
#' \item On \code{"resource"} level, this metric can give insights into which resources needs to repeat their work most often
#' within a case, or for which resource the work they did should be redone by another resource within the same case.
#' This metric shows the absolute and relative number of both repeat and redo self-loops for each resource in the log.
#' \item On \code{"resource-activity"} level, this metric can be used to get an insight in which activities are the most
#' crucial for which resources. This metric shows the absolute and relative number of both repeat and redo self-loops for
#' each of the resource-activity combinations that occur in the log. Two different relative numbers are provided here,
#' one from the resource perspective and one from the activity perspective. At the resource perspective, the denominator
#' is the total number of executions by the resource under consideration. At the activity perspective, the denominator
#' is the total number of occurrences of the activity under consideration.
#' }
#'
#' @inherit number_of_repetitions params
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link{number_of_repetitions}}
#'
#' @family metrics
#'
#' @concept metrics_repetition
#'
#' @export number_of_selfloops
number_of_selfloops <- function(log,
								type = c("all", "repeat", "redo"),
								level = c("log", "case", "activity", "resource", "resource-activity"),
								sort = TRUE) {
	UseMethod("number_of_selfloops")
}

#' @describeIn number_of_selfloops Computes the number of self-loops for an \code{\link[bupaR]{eventlog}}.
#' @export
number_of_selfloops.eventlog <- function(log,
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
					  log = all_selfloops_log,
					  case = all_selfloops_case,
					  activity = all_selfloops_activity,
					  resource = all_selfloops_resource,
					  "resource-activity" = all_selfloops_resource_activity
		)
	}
	else if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_selfloops_log,
					  case = repeat_selfloops_case,
					  activity = repeat_selfloops_activity,
					  resource = repeat_selfloops_resource,
					  "resource-activity" = repeat_selfloops_resource_activity
		)

	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_selfloops_log,
					  case = redo_selfloops_case,
					  activity = redo_selfloops_activity,
					  resource = redo_selfloops_resource,
					  "resource-activity" = redo_selfloops_resource_activity
		)
	}

	output <- FUN(eventlog = log)
	if(sort && level %in% c("case","resource", "activity","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric_v2(log, output, level, "number_of_selfloops")

	attr(output, "type") <- type

	return(output)
}

#' @describeIn number_of_selfloops Computes the number of self-loops for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
number_of_selfloops.grouped_eventlog <- function(log,
												 type = c("all", "repeat", "redo"),
												 level = c("log", "case", "activity", "resource", "resource-activity"),
												 sort = TRUE) {

	type <- rlang::arg_match(type)
	level <- rlang::arg_match(level)

	absolute <- NULL

	if(type == "all") {
		FUN <- switch(level,
					  log = all_selfloops_log,
					  case = all_selfloops_case,
					  activity = all_selfloops_activity,
					  resource = all_selfloops_resource,
					  "resource-activity" = all_selfloops_resource_activity
		)
	}
	else if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_selfloops_log,
					  case = repeat_selfloops_case,
					  activity = repeat_selfloops_activity,
					  resource = repeat_selfloops_resource,
					  "resource-activity" = repeat_selfloops_resource_activity
		)

	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_selfloops_log,
					  case = redo_selfloops_case,
					  activity = redo_selfloops_activity,
					  resource = redo_selfloops_resource,
					  "resource-activity" = redo_selfloops_resource_activity
		)
	}

	output <- bupaR:::apply_grouped_fun(log, fun = FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	#	if(level != "log") {
	#		output <- grouped_metric(eventlog, FUN)
	#	}
	#	else {
	#		output <- grouped_metric_raw_log(eventlog, FUN)
	#	}

	if(sort && level %in% c("case", "resource", "activity", "resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric_v2(log, output, level, "number_of_selfloops")

	attr(output, "type") <- type

	return(output)
}

#' @describeIn number_of_selfloops Computes the number of self-loops for an \code{\link[bupaR]{activitylog}}.
#' @export
number_of_selfloops.activitylog <- function(log,
											type = c("all", "repeat", "redo"),
											level = c("log", "case", "activity", "resource", "resource-activity"),
											sort = TRUE) {

	type <- rlang::arg_match(type)
	level <- rlang::arg_match(level)

	number_of_selfloops.eventlog(bupaR::to_eventlog(log),
								 type = type,
								 level = level,
								 sort = sort)
}

#' @describeIn number_of_selfloops Computes the number of self-loops for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
number_of_selfloops.grouped_activitylog <- function(log,
													type = c("all", "repeat", "redo"),
													level = c("log", "case", "activity", "resource", "resource-activity"),
													sort = TRUE) {


	type <- rlang::arg_match(type)
	level <- rlang::arg_match(level)

	number_of_selfloops.grouped_eventlog(bupaR::to_eventlog(log),
										 type = type,
										 level = level,
										 sort = sort)
}
