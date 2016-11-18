#' @title Metric: Size of repetitions
#'
#' @description Provides summary statistics on the sizes of repetitions at the level of activity types, cases, traces or log. A selfloop of size x refers to the occurrence of x consecutive events
#' of that activity type.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type Type of repetitions, repeat or redo.
#'
#' @param level_of_analysis At which level the analysis of selfloops should be performed: log, case, activity, resource, resource-activity.
#'
#' @param raw Return raw data (only applicable for log level)
#'
#' @export size_of_repetitions

size_of_repetitions <- function(eventlog,
							  type,
							  level_of_analysis,
								raw = F){

	stop_eventlog(eventlog)

	if(!(type %in% c("repeat","redo")))
		stop("Type should be \"repeat\" or \"redo\"")

	if(!(level_of_analysis %in% c("activity", "case","log", "resource","resource-activity")))
		stop("Level of analysis should be one of the following: activity, case, log, resource, resource-activity.")



	if(type == "repeat") {
		switch(level_of_analysis,
			   log = repeat_repetitions_size_log(eventlog, raw),
			   case = repeat_repetitions_size_case(eventlog),
			   activity = repeat_repetitions_size_activity(eventlog),
			   resource = repeat_repetitions_size_resource(eventlog),
			   "resource-activity" = repeat_repetitions_size_resource_activity(eventlog)
		)
	}
	else if (type == "redo") {
		switch(level_of_analysis,
			   log = redo_repetitions_size_log(eventlog, raw),
			   case = redo_repetitions_size_case(eventlog),
			   activity = redo_repetitions_size_activity(eventlog),
			   resource = redo_repetitions_size_resource(eventlog),
			   "resource-activity" = redo_repetitions_size_resource_activity(eventlog)
		)

	}
}
