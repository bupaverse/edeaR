#'
#'  Metric: Start activities
#'
#'
#' Analyse the start activities in the process
#'
#' \itemize{
#' \item On log levels, this metric shows the absolute and relative number of activities that are the first activity in one or more of the cases.
#' \item On the level of the specific cases, this metric provides an overview of the start activity of each case.
#' \item  On the activity level This metric calculates for each activity the absolute and relative number of cases that start with this activity type.
#'The relative number is calculated as a portion of the
#'number of cases, being the number of "opportunities" that an activity could be the start activity.
#'The cumulative sum is added to have an insight in the number of activities that is required to cover a certain part of the total.
#' \item On the level of the distinct resources, an overview of which resources execute the first activity per case can be of interest for a
#'company. Probably this person is responsible for the correct communication to the customer.
#'\item  Finally, on the resource-activity level,
#'this metric shows for each occurring resource-activity combination the absolute and relative number of times this resource executes this
#'activity as an start activity in a case.}

#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#'
#'@export start_activities

start_activities <- function(eventlog, level, append, ...) {
	UseMethod("start_activities")
}

#' @describeIn start_activities Start activities for eventlog
#' @export

start_activities.eventlog <- function(eventlog,
							 level = c("log","case","activity","resource","resource-activity"),
							 append = FALSE,
							 append_column = NULL,
							 sort = TRUE,
							 ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
	absolute <- NULL
	if(is.null(append_column)) {
		append_column <- case_when(level == "activity" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity" ~ "absolute",
								   level == "case" ~ activity_id(eventlog),
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = start_activities_log,
				  case = start_activities_case,
				  activity = start_activities_activity,
				  resource = start_activities_resource,
				  "resource-activity" = start_activities_resource_activity)

	output <- FUN(eventlog = eventlog)
	if(sort && level %in% c("activity", "resource","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append, append_column, "start_activities", ifelse(level == "case",1,3))

}

#' @describeIn start_activities Start activities for grouped eventlog
#' @export

start_activities.grouped_eventlog <- function(eventlog,
							 level = c("log","case","activity","resource","resource-activity"),
							 append = FALSE,
							 append_column = NULL,
							 sort = TRUE,
							 ...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = start_activities_log,
				  case = start_activities_case,
				  activity = start_activities_activity,
				  resource = start_activities_resource,
				  "resource-activity" = start_activities_resource_activity)

	grouped_metric(eventlog, FUN) -> output
	if(sort && level %in% c("activity", "resource","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append,append_column, "start_activities", ifelse(level == "case",1,3))

}
