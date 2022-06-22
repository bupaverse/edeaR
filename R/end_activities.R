#' @title End activities
#'
#' @description Analyse the end activities in the process.
#'
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"}, \code{"case"}, \code{"activity"}, \code{"resource"}, or \code{"resource-activity"}.
#' For more information, see \code{vignette("metrics", "edeaR")} and 'Details' below.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{log} level, this metric shows the absolute and relative number of activities that are the last activity in one or more of the cases.
#' \item On \code{case} level, this metric provides an overview of the end activity of each case.
#' \item On \code{activity} level, this metric calculates for each activity the absolute and relative number of cases that end with this activity type.
#' Similar to the \code{\link{start_activities}} metric, the relative number is calculated as a portion of the number of cases, being the number of "opportunities" that an activity could be the end activity.
#' The cumulative sum is added to have an insight in the number of activities that is required to cover a certain part of the total.
#' \item At \code{resource} level, an overview of which resources execute the last activity per case can be of interest for a company.
#' Probably this person is responsible for the correct communication to the customer.
#' \item On \code{resource-activity} level, this metric shows for each occurring resource-activity combination the absolute and relative number of times
#' this resource executes this activity as an end activity in a case.
#' }
#'
#' @inherit activity_frequency params references return seealso
#'
#' @family metrics
#'
#' @export end_activities
end_activities <- function(log, level = c("log", "case", "activity", "resource", "resource-activity"), append = deprecated(), append_column = NULL, sort = TRUE, ..., eventlog = deprecated())  {
	UseMethod("end_activities")
}

#' @describeIn end_activities Compute end activities for an \code{\link[bupaR]{eventlog}}.
#' @export
end_activities.eventlog <- function(log,
									level = c("log", "case", "activity", "resource", "resource-activity"),
									append = deprecated(),
									append_column = NULL,
									sort = TRUE,
									...,
									eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	if(is.null(append_column)) {
		append_column <- case_when(level == "activity" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity" ~ "absolute",
								   level == "case" ~ activity_id(log),
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = end_activities_log,
				  case = end_activities_case,
				  activity = end_activities_activity,
				  resource = end_activities_resource,
				  "resource-activity" = end_activities_resource_activity)

	output <- FUN(eventlog = log)

	if(sort && level %in% c("activity", "resource","resource-activity")) {
		output %>%
			arrange(-.data[["absolute"]]) -> output
	}

	return_metric(log, output, level, append, append_column, "end_activities", n_result_col = ifelse(level == "case",1,3))
}

#' @describeIn end_activities Compute end activities for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
end_activities.grouped_eventlog <- function(log,
											level = c("log", "case", "activity", "resource", "resource-activity"),
											append = deprecated(),
											append_column = NULL,
											sort = TRUE,
											...,
											eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	if(is.null(append_column)) {
		append_column <- case_when(level == "activity" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity" ~ "absolute",
								   level == "case" ~ activity_id(log),
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = end_activities_log,
				  case = end_activities_case,
				  activity = end_activities_activity,
				  resource = end_activities_resource,
				  "resource-activity" = end_activities_resource_activity)

	output <- grouped_metric(log, FUN)

	if(sort && level %in% c("activity", "resource", "resource-activity")) {
		output %>%
			arrange(-.data[["absolute"]])-> output
	}

	return_metric(log, output, level, append, append_column, "end_activities", n_result_col = ifelse(level == "case",1,3))
}

#' @describeIn end_activities Compute end activities for an \code{\link[bupaR]{activitylog}}.
#' @export
end_activities.activitylog <- function(log,
									   level = c("log", "case", "activity", "resource", "resource-activity"),
									   append = deprecated(),
									   append_column = NULL,
									   sort = TRUE,
									   ...,
									   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	end_activities.eventlog(bupaR::to_eventlog(log), level = level, append = append, append_column = append_column, sort = sort, ...)
}

#' @describeIn end_activities Compute end activities for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
end_activities.grouped_activitylog <- function(log,
											   level = c("log", "case", "activity", "resource", "resource-activity"),
											   append = deprecated(),
											   append_column = NULL,
											   sort = TRUE,
											   ...,
											   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)
	level <- rlang::arg_match(level)

	end_activities.grouped_eventlog(bupaR::to_eventlog(log), level = level, append = append, append_column = append_column, sort = sort, ...)
}