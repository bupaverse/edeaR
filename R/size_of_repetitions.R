#' @title Metric: Size of repetitions
#'
#' @description Provides summary statistics on the sizes of repetitions.
#'
#'
#' @inherit end_activities params
#' @inherit number_of_selfloops params
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link{number_of_repetitions}}
#'
#' @concept metrics_repetition
#'
#' @export size_of_repetitions
#'
size_of_repetitions <- function(log,
								type = c("all","repeat","redo"),
								level = c("log","case","activity","resource","resource-activity")) {
	UseMethod("size_of_repetitions")
}
#' @describeIn size_of_repetitions Size of repetitions for eventlog
#' @export

size_of_repetitions.eventlog <- function(log,
								type = c("all","repeat","redo"),
								level = c("log","case","activity","resource","resource-activity")){


	if(all((type) == c("all", "repeat","redo")))
		message("Using default type: all")
	if(all((level) == c("log","case","activity","resource","resource-activity")))
		message("Using default level: log")

	type <- match.arg(type)
	level <- match.arg(level)

	if(type == "all") {
		FUN <- switch(level,
					  log = all_repetitions_size_log,
					  case = all_repetitions_size_case,
					  activity = all_repetitions_size_activity,
					  resource = all_repetitions_size_resource,
					  "resource-activity" = all_repetitions_size_resource_activity
		)
	}
	else if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_size_log,
					  case = repeat_repetitions_size_case,
					  activity = repeat_repetitions_size_activity,
					  resource = repeat_repetitions_size_resource,
					  "resource-activity" = repeat_repetitions_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- 	switch(level,
					   log = redo_repetitions_size_log,
					   case = redo_repetitions_size_case,
					   activity = redo_repetitions_size_activity,
					   resource = redo_repetitions_size_resource,
					   "resource-activity" = redo_repetitions_size_resource_activity
		)

	}

	output <- FUN(eventlog = log)


	output <- return_metric_v2(log, output, level, "size_of_repetitions")
	attr(output, "type") <- type
	return(output)
}

#' @describeIn size_of_repetitions Size of repetitions for grouped event log
#' @export

size_of_repetitions.grouped_eventlog <- function(log,
												 type = c("repeat","redo"),
												 level = c("log","case","activity","resource","resource-activity")){


	type <- match.arg(type)
	level <- match.arg(level)

	if(type == "repeat") {
		FUN <- switch(level,
					  log = repeat_repetitions_size_log,
					  case = repeat_repetitions_size_case,
					  activity = repeat_repetitions_size_activity,
					  resource = repeat_repetitions_size_resource,
					  "resource-activity" = repeat_repetitions_size_resource_activity
		)
	}
	else if (type == "redo") {
		FUN <- 	switch(level,
					   log = redo_repetitions_size_log,
					   case = redo_repetitions_size_case,
					   activity = redo_repetitions_size_activity,
					   resource = redo_repetitions_size_resource,
					   "resource-activity" = redo_repetitions_size_resource_activity
		)

	}

	output <- bupaR:::apply_grouped_fun(log, FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	output <- return_metric_v2(log, output, level,"size_of_repetitions")
	attr(output, "type") <- type
	return(output)
}

#' @describeIn size_of_repetitions Size of repetitions for activitylog
#' @export

size_of_repetitions.activitylog <- function(log,
												 type = c("repeat","redo"),
												 level = c("log","case","activity","resource","resource-activity")){


	log %>% to_eventlog %>% size_of_repetitions(type, level)
}

#' @describeIn size_of_repetitions Size of repetitions for grouped activitylog
#' @export

size_of_repetitions.grouped_activitylog <- function(log,
												 type = c("repeat","redo"),
												 level = c("log","case","activity","resource","resource-activity")){


	size_of_repetitions.grouped_eventlog(bupaR::to_eventlog(log),
									   type = type,
									   level = level)
}
