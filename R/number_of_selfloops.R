#'  Metric: Number of selfloops in trace
#'
#' Provides information statistics on the number of selfloops.
#'
#' Activity instances of the same activity type that are executed more than
#' once immediately after each other by the same resource are in a self-loop (length-1-loop).
#' If an activity instance of the same activity type is executed 3 times
#' after each other by the same resource, this is defined as a size 2 self-loop.
#'
#' Two types of self-loops are defined, which are repeat self-loops and redo self-loops. Repeat self-loops are
#' activity executions of the same activity type that are executed immediately following
#' each other by the same resource. Redo self-loops are activity executions of the
#' same activity type that are executed immediately following each other by a different
#' resource. Repeat and redo repetitions are explained further on.
#'
#' These metrics are presented on five different levels of analysis, which are the
#' complete event log, cases, activities, resources and resource-activity combinations.
#'
#' \itemize{
#'
#' \item On the level of the complete event log, the summary statistics of the
#' number of self-loops within a trace can give a first insight in the amount of
#' waste in an event log. As stated earlier, each combination of two occurrences of
#' the same activity executed by the same resource will be counted as one repeat
#' 	self-loop of this activity.
#'
#' 	\item This metric on the level of cases provides
#' 	an overview of the absolute and relative number of repeat and redo self-loops
#' 	in each case.  To calculate the relative number, each (repeat or
#' 	 redo) self-loop is counted as 1 occurrence, and the other
#' 	 activity instances are also counted as 1.
#'
#' 	 \item On the level of the distinct activities in the event log, the absolute and relative number of self-loops per
#' 	 activity can be an indication for the company which activities are causing the
#' 	 most waste in the process.
#'
#' 	 \item Similar to the metric on the level of the
#' 	 activities, the number of self-loops on the level of the resources executing the
#' 	 activities can give a company insights in which employee needs to repeat his or
#' 	 her work most often within a case, or for which employee the work he or she
#' 	 did should be redone by another employee within the same case. This metric
#' 	 shows the absolute and relative number of both repeat and redo self-loops for
#' 	 each resource in the event log.
#'
#'  \item Finally, the metric can be applied
#'  to the level of the specifc resource-activity combinations, in order to get an
#'  insight in which activities are the most crucial for which resources. This metric
#'  shows the absolute and relative number of both repeat and redo self-loops for
#'  each of the resource-activity combinations that occur in the event log. Two
#'  different relative numbers are provided here, one from the resource perspective
#'  and one from the activity perspective. At the resource perspective, the denominator
#'  is the total number of executions by the resource under consideration.
#'  At the activity perspective, the denominator is the total number of occurrences
#'  of the activity under consideration.
#'
#'
#'
#' }
#'
#'
#' @param type The type of repetitions, either all, repeat or redo.
#'
#'
#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#' @export number_of_selfloops

number_of_selfloops <- function(eventlog, type, level, append, ...) {
	UseMethod("number_of_selfloops")
}

#' @describeIn number_of_selfloops Compute number of selfloops for eventlog
#' @export

number_of_selfloops.eventlog <- function(eventlog,
								type = c("all", "repeat","redo"),
								level = c("log","case","activity","resource","resource-activity"),
								append = FALSE,
								append_column = NULL,
								sort = TRUE,
								...) {
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

	output <- FUN(eventlog = eventlog)
	if(sort && level %in% c("case","resource", "activity","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric(eventlog, output, level, append, append_column, "number_of_selfloops")

	attr(output, "type") <- type

	return(output)

}

#' @describeIn number_of_selfloops Compute number of selfloops for grouped eventlog
#' @export

number_of_selfloops.grouped_eventlog <- function(eventlog,
								type = c("all", "repeat","redo"),
								level = c("log","case","activity","resource","resource-activity"),
								append = FALSE,
								append_column = NULL,
								sort = TRUE,
								...) {
	absolute <- NULL
	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   level == "activity"~"absolute",
								   T ~ "NA")
	}

	type <- match.arg(type)
	level <- match.arg(level)
	level <- deprecated_level(level, ...)


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

		if(level != "log") {
			output <- grouped_metric(eventlog, FUN)
		}
		else {
			output <- grouped_metric_raw_log(eventlog, FUN)
		}

	if(sort && level %in% c("case","resource", "activity","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	output <- return_metric(eventlog, output, level, append, append_column, "number_of_selfloops", ifelse(level == "resource-activity", 3,2))

	attr(output, "type") <- type

	return(output)

}

