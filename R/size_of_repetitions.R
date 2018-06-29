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
#' @export size_of_repetitions
#'
size_of_repetitions <- function(eventlog, type, level, append, ...) {
	UseMethod("size_of_repetitions")
}
#' @describeIn size_of_repetitions Size of repetitions for eventlog
#' @export

size_of_repetitions.eventlog <- function(eventlog,
								type = c("all","repeat","redo"),
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

	output <- FUN(eventlog = eventlog)


	output <- return_metric(eventlog, output, level, append, append_column, "size_of_repetitions", 10)
	attr(output, "type") <- type
	return(output)
}

#' @describeIn size_of_repetitions Size of repetitions for grouped event log
#' @export

size_of_repetitions.grouped_eventlog <- function(eventlog,
												 type = c("repeat","redo"),
												 level = c("log","case","activity","resource","resource-activity"),
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

	if(level != "log") {
		output <- grouped_metric(eventlog, FUN)
	}
	else {
		output <- grouped_metric_raw_log(eventlog, FUN)
	}


	output <- return_metric(eventlog, output, level, append, append_column, "size_of_repetitions", 10)
	attr(output, "type") <- type
	return(output)
}
