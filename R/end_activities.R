#' @title End activities
#'
#' @description Analyse the end activities in the process.
#'
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"} (default),
#' \code{"case"}, \code{"activity"}, \code{"resource"}, or \code{"resource-activity"}. For more information,
#' see \code{vignette("metrics", "edeaR")} and 'Details' below.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{log} level, this metric shows the absolute and relative number of activities that are the last activity
#' in one or more of the cases.
#' \item On \code{case} level, this metric provides an overview of the end activity of each case.
#' \item On \code{activity} level, this metric calculates for each activity the absolute and relative number of cases that
#' end with this activity type. Similar to the \code{\link{start_activities}} metric, the relative number is calculated
#' as a portion of the number of cases, being the number of "opportunities" that an activity could be the end activity.
#' The cumulative sum is added to have an insight in the number of activities that is required to cover a certain part of the total.
#' \item At \code{resource} level, an overview of which resources execute the last activity per case is provided.
#' \item On \code{resource-activity} level, this metric shows for each occurring resource-activity combination the absolute
#' and relative number of times this resource executes this activity as an end activity in a case.
#' }
#'
#' @inherit activity_frequency params references return seealso
#'
#' @seealso \code{\link{start_activities}}
#'
#' @family metrics
#'
#' @concept metrics_structuredness
#'
#' @export end_activities
end_activities <- function(log,
						   level = c("log", "case", "activity", "resource", "resource-activity"),
						   sort = TRUE)  {
	UseMethod("end_activities")
}

#' @describeIn end_activities Computes the end activities for an \code{\link[bupaR]{eventlog}}.
#' @export
end_activities.eventlog <- function(log,
									level = c("log", "case", "activity", "resource", "resource-activity"),
									sort = TRUE) {


	level <- rlang::arg_match(level)

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

	return_metric_v2(log, output, level, "end_activities")
}

#' @describeIn end_activities Computes the end activities for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
end_activities.grouped_eventlog <- function(log,
											level = c("log", "case", "activity", "resource", "resource-activity"),
											sort = TRUE) {

	level <- rlang::arg_match(level)

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

	return_metric_v2(log, output, level, "end_activities")
}

#' @describeIn end_activities Computes the end activities for an \code{\link[bupaR]{activitylog}}.
#' @export
end_activities.activitylog <- function(log,
									   level = c("log", "case", "activity", "resource", "resource-activity"),
									   sort = TRUE) {

	level <- rlang::arg_match(level)

	end_activities.eventlog(bupaR::to_eventlog(log),
							level = level,
							sort = sort)
}

#' @describeIn end_activities Computes the end activities for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
end_activities.grouped_activitylog <- function(log,
											   level = c("log", "case", "activity", "resource", "resource-activity"),
											   sort = TRUE) {

	level <- rlang::arg_match(level)

	end_activities.grouped_eventlog(bupaR::to_eventlog(log),
									level = level,
									sort = sort)
}
