#' @title Start Activities
#'
#' @description Analyse the start activities in the process.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item On \code{"log"} level, this metric shows the absolute and relative number of activities that are the first activity
#' in one or more of the cases.
#' \item On \code{"case"} level, this metric provides an overview of the start activity of each case.
#' \item On \code{"activity"} level, this metric calculates for each activity the absolute and relative number of cases
#' that start with this activity type. Similar to the \code{\link{end_activities}} metric, the relative number is calculated
#' as a portion of the number of cases, being the number of "opportunities" that an activity could be the start activity.
#' The cumulative sum is added to have an insight in the number of activities that is required to cover a certain part of the total.
#' \item On \code{"resource"} level, an overview of which resources execute the first activity per case are provided.
#' \item On \code{"resource-activity"} level, this metric shows for each occurring resource-activity combination the absolute
#' and relative number of times this resource executes this activity as an start activity in a case.
#' }
#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link{end_activities}}
#'
#' @family metrics
#'
#' @concept metrics_structuredness
#'
#' @export start_activities
start_activities <- function(log,
							 level = c("log", "case", "activity", "resource", "resource-activity"),
							 append = deprecated(),
							 append_column = NULL,
							 sort = TRUE,
							 eventlog = deprecated()) {
	UseMethod("start_activities")
}

#' @describeIn start_activities Computes the start activities for an \code{\link[bupaR]{eventlog}}.
#' @export
start_activities.eventlog <- function(log,
									  level = c("log", "case", "activity", "resource", "resource-activity"),
									  append = deprecated(),
									  append_column = NULL,
									  sort = TRUE,
									  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	if(is.null(append_column)) {
		append_column <- case_when(level == "activity" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity" ~ "absolute",
								   level == "case" ~ activity_id(log),
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = start_activities_log,
				  case = start_activities_case,
				  activity = start_activities_activity,
				  resource = start_activities_resource,
				  "resource-activity" = start_activities_resource_activity)

	output <- FUN(log = log)

	if(sort && level %in% c("activity", "resource", "resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "start_activities", ifelse(level == "case",1,3))
}

#' @describeIn start_activities Computes the start activities for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
start_activities.grouped_eventlog <- function(log,
											  level = c("log", "case", "activity", "resource", "resource-activity"),
											  append = deprecated(),
											  append_column = NULL,
											  sort = TRUE,
											  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	FUN <- switch(level,
				  log = start_activities_log,
				  case = start_activities_case,
				  activity = start_activities_activity,
				  resource = start_activities_resource,
				  "resource-activity" = start_activities_resource_activity)

	output <- bupaR:::apply_grouped_fun(log, fun = FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	if(sort && level %in% c("activity", "resource", "resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "start_activities", ifelse(level == "case",1,3))
}

#' @describeIn start_activities Computes the start activities for an \code{\link[bupaR]{activitylog}}.
#' @export
start_activities.activitylog <- function(log,
										 level = c("log", "case", "activity", "resource", "resource-activity"),
										 append = deprecated(),
										 append_column = NULL,
										 sort = TRUE,
										 eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	start_activities.eventlog(bupaR::to_eventlog(log),
							  level = level,
							  append = append,
							  append_column = append_column,
							  sort = sort)
}

#' @describeIn start_activities Computes the start activities for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
start_activities.grouped_activitylog <- function(log,
												 level = c("log", "case", "activity", "resource", "resource-activity"),
												 append = deprecated(),
												 append_column = NULL,
												 sort = TRUE,
												 eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	start_activities.grouped_eventlog(bupaR::to_eventlog(log),
									  level = level,
									  append = append,
									  append_column = append_column,
									  sort = sort)
}
