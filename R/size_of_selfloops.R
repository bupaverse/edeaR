#' @title Metric: Size of selfloops
#'
#' @description Provides summary statistics on the sizes of selfloops at the level of activity types, cases, traces or log. A selfloop of size x refers to the occurrence of x consecutive events
#' of that activity type.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param type Type of selfloops. I.e. repeat or redo
#'
#' @param level_of_analysis At which level the analysis of selfloops should be performed: log, case, activity, resource, resource activity.
#'
#' @param raw Return raw data (only applicable for log level)
#'
#' @export size_of_selfloops

size_of_selfloops <- function(eventlog,
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
			   log = repeat_selfloops_size_log(eventlog, raw),
			   case = repeat_selfloops_size_case(eventlog),
			   activity = repeat_selfloops_size_activity(eventlog),
			   resource = repeat_selfloops_size_resource(eventlog),
			   "resource-activity" = repeat_selfloops_size_resource_activity(eventlog)
		)
	}
	else if (type == "redo") {
		switch(level_of_analysis,
			   log = redo_selfloops_size_log(eventlog, raw),
			   case = redo_selfloops_size_case(eventlog),
			   activity = redo_selfloops_size_activity(eventlog),
			   resource = redo_selfloops_size_resource(eventlog),
			   "resource-activity" = redo_selfloops_size_resource_activity(eventlog)
		)

	}
}
