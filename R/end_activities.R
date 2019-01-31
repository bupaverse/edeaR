#' Metric: End activities
#'
#' Analyse the end activities in the process.
#'
#' \itemize{
#' \item On log levels, this metric shows the absolute and relative number of activities that are the last activity in one or more of the cases.
#' \item On the level of the specific cases, this metric provides an overview of the end activity of each case.
#' \item  On the activity level This metric calculates for each activity the absolute and relative number of cases that end with this activity type.
#'Similar to the start activities metric, the relative number is calculated as a portion of the
#'number of cases, being the number of \"opportunities\" that an activity could be the end activity.
#'The cumulative sum is added to have an insight in the number of activities that is required to cover a certain part of the total.
#' \item On the level of the distinct resources, an overview of which resources execute the last activity per case can be of interest for a
#'company. Probably this person is responsible for the correct communication to the customer.
#'\item  Finally, on the resource-activity level,
#'this metric shows for each occurring resource-activity combination the absolute and relative number of times this resource executes this
#'activity as an end activity in a case.}

#'
#' @param level Level of granularity for the analysis: log,  case, activity, resource or resource-activity.
#' For more information, see \code{vignette("metrics", "edeaR")}
#'
#' @inherit activity_frequency params references return seealso

#' @export end_activities

end_activities <- function(eventlog, level, append,  ...) {
	UseMethod("end_activities")
}

#' @describeIn end_activities Compute end activities for eventlog
#' @export

end_activities.eventlog <- function(eventlog,
									level = c("log","case","activity","resource","resource-activity"),
									append = FALSE,
									append_column = NULL,
									sort = TRUE,
									...) {
	absolute <- NULL

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "activity" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity" ~ "absolute",
								   level == "case" ~ activity_id(eventlog),
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = end_activities_log,
				  case = end_activities_case,
				  activity = end_activities_activity,
				  resource = end_activities_resource,
				  "resource-activity" = end_activities_resource_activity)

	output <- FUN(eventlog = eventlog)
	if(sort && level %in% c("activity", "resource","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append, append_column, "end_activities", n_result_col = ifelse(level == "case",1,3))

}

#' @describeIn end_activities Compute end activities for grouped eventlog
#' @export

end_activities.grouped_eventlog <- function(eventlog,
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
				  log = end_activities_log,
				  case = end_activities_case,
				  activity = end_activities_activity,
				  resource = end_activities_resource,
				  "resource-activity" = end_activities_resource_activity)

	output <- grouped_metric(eventlog, FUN)
	if(sort && level %in% c("activity", "resource","resource-activity")) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append, append_column, "end_activities", n_result_col = ifelse(level == "case",1,3))

}
