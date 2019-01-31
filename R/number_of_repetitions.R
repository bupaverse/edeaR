
#' Metric:  Number of repetitions
#'
#' Provides information statistics on the number of repetitions
#'
#'
#' A repetition is an execution
#' of an activity within a case while that activity has already been executed before, but
#' one or more other activities are executed in between.Similar to the self-loop metric, a distinction
#' should be made between repeat and redo repetitions.
#' Repeat repetitions are activity executions of the same activity type that are executed
#' not immediately following each other, but by the same resource. Redo repetitions
#' are activity executions of the same activity type that are executed not immediately
#' following each other and by a different resource than the first activity occurrence of
#' this activity type.
#'
#'
#' \itemize{
#'
#' \item The number of repetitions can be calculated on the level of the complete event log. This metric shows the
#' summary statistics of the number of repetitions within a case, which can provide
#' insights in the amount of waste in an event log. Each combination of two or more
#' occurrences of the same activity, executed not immediately following each other,
#' by the same resource is counted as one repeat repetition of this activity.
#'
#' \item On case level, this metric provides the absolute and relative number of repetitions in each case.
#'
#' \item On the level of specific activities, this metric shows which activities occur the most in a repetition.
#' The absolute and relative number of both repeat and redo repetitions is
#' provided by this metric, giving an overview per activity.
#'
#' \item When looking at the different resources executing activities in the event log, it can be interesting to have an overview
#' of which resources need more than one time to execute an activity in a case or
#' which resources need to have an activity redone later on in the case by another
#' resource. This metric provides the absolute and relative number of times each
#' resource appears in a repetition.
#'
#' \item Finally, the same metric can be
#' looked at on the level of specific resource-activity combinations, providing the
#'  company with specific information about which activities and which resources
#'  are involved in the repetitions. For this metric the absolute and relative number
#'   of repeat and redo repetitions is provided. Again two difierent relative numbers
#'   are provided, one relative to the total number of executions of the activity in the
#'   complete event log, and one relative to the total number of executions performed
#'   by the resource throughout the complete event log.
#' }
#'
#' @param type The type of repetitions, either repeat or redo.
#'
#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#'
#' @export number_of_repetitions


number_of_repetitions <- function(eventlog, type, level, append, ...) {
	UseMethod("number_of_repetitions")
}

#' @describeIn number_of_repetitions Apply metric on event log
#' @export

number_of_repetitions.eventlog <- function(eventlog,
								  type = c("all","repeat","redo"),
								  level = c("log","case","activity","resource","resource-activity"),
								  append = FALSE,
								  append_column = NULL,
								  sort = TRUE,
								  ...){
	absolute <- NULL
	if(all((type) == c("all", "repeat","redo")))
		message("Using default type: all")
	if(all((level) == c("log","case","activity","resource","resource-activity")))
		message("Using default level: log")

	type <- match.arg(type)
	level <- match.arg(level)

	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   level == "activity"~"absolute",
								   T ~ "NA")
	}

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

	output <- FUN(eventlog = eventlog)


	if(sort && level %in% c("case","resource", "activity","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric(eventlog, output, level, append, append_column, "number_of_repetitions", ifelse(level == "resource-activity", 3,2))
	attr(output, "type") <- type

	return(output)

}


#' @describeIn number_of_repetitions Apply metric on grouped eventlog
#' @export

number_of_repetitions.grouped_eventlog <- function(eventlog,
												   type = c("repeat","redo"),
												   level = c("log","case","activity","resource","resource-activity"),
												   append = F,
												   append_column = NULL,
												   sort = TRUE,
												   ...){

	absolute <- NULL

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   level == "activity"~"absolute",
								   T ~ "NA")
	}

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

	if(level != "log") {
		grouped_metric(eventlog, FUN) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN) -> output
	}


	if(sort && level %in% c("case","resource", "activity","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric(eventlog, output, level, append, append_column, "number_of_repetitions", ifelse(level == "resource-activity", 3,2))
	attr(output, "type") <- type

	return(output)

}
