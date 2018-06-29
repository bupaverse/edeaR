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
#' @export size_of_selfloops

size_of_selfloops <- function(eventlog, type, level, append, ...) {
	UseMethod("size_of_selfloops")
}

#' @describeIn size_of_selfloops Size of selfloops for eventlog
#' @export

size_of_selfloops.eventlog <- function(eventlog,
									   type = c("all", "repeat","redo"),
									   level = c("log","case","activity","resource","resource-activity"),
									   append = FALSE,
									   append_column = NULL,
									   ...){


	if(all((type) == c("all", "repeat","redo")))
		message("Using default type: all")
	if(all((level) == c("log","case","activity","resource","resource-activity")))
		message("Using default level: log")

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "median",
								   level == "activity"~"median",
								   level == "resource-activity"~"median",
								   T ~ "NA")
	}

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

	output <- FUN(eventlog = eventlog)

	output <- return_metric(eventlog, output, level, append, append_column, "size_of_selfloops", 10)
	attr(output, "type") <- type
	return(output)
}

#' @describeIn size_of_selfloops Size of selfloops for grouped eventlog
#' @export

size_of_selfloops.grouped_eventlog <- function(eventlog,
											   type = c("repeat","redo"),
											   level = c("log","case","activity","resource","resource-acitivty"),
											   append = FALSE,
											   append_column = NULL,
											   ...){

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "median",
								   level == "activity"~"median",
								   level == "resource-activity"~"median",
								   T ~ "NA")
	}

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

	if(level != "log") {
		grouped_metric(eventlog, FUN) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN) -> output
	}


	output <- return_metric(eventlog, output, level, append, append_column, "size_of_selfloops", 10)
	attr(output, "type") <- type
	return(output)
}
