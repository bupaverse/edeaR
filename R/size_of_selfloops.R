#' @title Metric: Size of selfloops
#'
#' @description Provides summary statistics on the sizes of selfloops
#'
#'
#' @inherit end_activities params
#' @inherit number_of_selfloops params
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link{number_of_selfloops}}
#'
#' @concept metrics_repetition
#'
#' @export size_of_selfloops

size_of_selfloops <- function(log,
							  type = c("all", "repeat","redo"),
							  level = c("log","case","activity","resource","resource-activity")) {
	UseMethod("size_of_selfloops")
}

#' @describeIn size_of_selfloops Size of selfloops for eventlog
#' @export

size_of_selfloops.eventlog <- function(log,
									   type = c("all", "repeat","redo"),
									   level = c("log","case","activity","resource","resource-activity")){

	if(all((type) == c("all", "repeat","redo")))
		message("Using default type: all")
	if(all((level) == c("log","case","activity","resource","resource-activity")))
		message("Using default level: log")

	type <- match.arg(type)
	level <- match.arg(level)


	if(type == "all") {
		FUN <- switch(level,
					  log = all_selfloops_size_log,
					  case = all_selfloops_size_case,
					  activity = all_selfloops_size_activity,
					  resource = all_selfloops_size_resource,
					  "resource-activity" = all_selfloops_size_resource_activity
		)
	}
	else if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_selfloops_size_log,
					  case = repeat_selfloops_size_case,
					  activity = repeat_selfloops_size_activity,
					  resource = repeat_selfloops_size_resource,
					  "resource-activity" = repeat_selfloops_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_selfloops_size_log,
					  case = redo_selfloops_size_case,
					  activity = redo_selfloops_size_activity,
					  resource = redo_selfloops_size_resource,
					  "resource-activity" = redo_selfloops_size_resource_activity
		)

	}

	output <- FUN(eventlog = log)

	output <- return_metric_v2(log, output, level, "size_of_selfloops")
	attr(output, "type") <- type
	return(output)
}

#' @describeIn size_of_selfloops Size of selfloops for grouped eventlog
#' @export

size_of_selfloops.grouped_eventlog <- function(log,
											   type = c("repeat","redo"),
											   level = c("log","case","activity","resource","resource-acitivty")){

	type <- match.arg(type)
	level <- match.arg(level)

	if(type == "all") {
		FUN <- switch(level,
					  log = all_selfloops_size_log,
					  case = all_selfloops_size_case,
					  activity = all_selfloops_size_activity,
					  resource = all_selfloops_size_resource,
					  "resource-activity" = all_selfloops_size_resource_activity
		)
	}
	else if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_selfloops_size_log,
					  case = repeat_selfloops_size_case,
					  activity = repeat_selfloops_size_activity,
					  resource = repeat_selfloops_size_resource,
					  "resource-activity" = repeat_selfloops_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- switch(level,
					  log = redo_selfloops_size_log,
					  case = redo_selfloops_size_case,
					  activity = redo_selfloops_size_activity,
					  resource = redo_selfloops_size_resource,
					  "resource-activity" = redo_selfloops_size_resource_activity
		)

	}

	output <- apply_grouped_fun(log, FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	output <- return_metric_v2(log, output, level,  "size_of_selfloops")
	attr(output, "type") <- type
	return(output)
}
#' @describeIn size_of_selfloops Size of selfloops for activitylog
#' @export
size_of_selfloops.activitylog <- function(log,
									   type = c("all", "repeat","redo"),
									   level = c("log","case","activity","resource","resource-activity")){

	log %>% to_eventlog %>% size_of_selfloops(type = type,
											  level = level)
}

#' @describeIn size_of_selfloops Size of selfloops for grouped activitylog
#' @export
size_of_selfloops.grouped_activitylog <- function(log,
										  type = c("all", "repeat","redo"),
										  level = c("log","case","activity","resource","resource-activity")){


	size_of_selfloops.grouped_eventlog(bupaR::to_eventlog(log),
									   type = type,
										level = level)
}
