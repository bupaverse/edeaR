#' @title Metric: Activity Presence
#'
#' @description Calculates for each activity type in what percentage of cases it is present.
#'
#' @details An indication of variance can be the presence of the activities in the different cases. This metric shows for each activity the absolute
#' number of cases in which each activity occurs together with its relative presence.
#'
#' @examples
#' \dontrun{
#' data <- data.frame(case = rep("A",5),
#' activity_id = c("A","B","C","D","E"),
#' activity_instance_id = 1:5,
#' lifecycle_id = rep("complete",5),
#' timestamp = 1:5,
#' resource = rep("resource 1", 5))
#'
#' log <- bupaR::eventlog(data,case_id = "case",
#' activity_id = "activity_id",
#' activity_instance_id = "activity_instance_id",
#' lifecycle_id = "lifecycle_id",
#' timestamp = "timestamp",
#' resource_id = "resource")
#'
#' activity_presence(log)
#' }
#'
#' @inherit activity_frequency params references return seealso
#'
#' @family metrics
#'
#' @concept metrics_structuredness
#'
#' @export activity_presence
activity_presence <- function(log, append = deprecated(), append_column = NULL, sort = TRUE, eventlog = deprecated()) {
	UseMethod("activity_presence")
}

#' @describeIn activity_presence Compute activity presence for an \code{\link[bupaR]{eventlog}}.
#' @export
activity_presence.eventlog <- function(log, append = deprecated(), append_column = NULL, sort = TRUE, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	# Deprecated code => to be removed
	#if(is.null(append_column)) {
	#	append_column <- case_when(level == "activity" ~ "absolute",
	#							   level == "case" ~ "absolute",
	#							   T ~ "NA")
	#}

	output <- activity_presence_FUN(log = log)

	if(sort) {
		output %>%
			arrange(-.data[["absolute"]]) -> output
	}

	return_metric(log, output, "activity", append, "absolute", "activity_presence")
}

#' @describeIn activity_presence Compute activity presence for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
activity_presence.grouped_eventlog <- function(log, append = deprecated(), append_column = NULL, sort = TRUE, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	output <- bupaR:::apply_grouped_fun(log, activity_presence_FUN)

	if(sort) {
		output %>%
			arrange(-.data[["absolute"]]) -> output
	}

	return_metric(log, output, "activity", append, "absolute", "activity_presence")
}

#' @describeIn activity_presence Compute activity presence for an \code{\link[bupaR]{activitylog}}.
#' @export
activity_presence.activitylog <- function(log, append = deprecated(), append_column = NULL, sort = TRUE, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	activity_presence.eventlog(bupaR::to_eventlog(log), append = append, append_column = append_column, sort = sort)
}

#' @describeIn activity_presence Compute activity presence for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
activity_presence.grouped_activitylog <- function(log, append = deprecated(), append_column = NULL, sort = TRUE, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	activity_presence.grouped_eventlog(bupaR::to_eventlog(log), append = append, append_column = append_column, sort = sort)
}


activity_presence_FUN <- function(log) {
	log %>%
		as.data.frame() %>%
		group_by(.data[[activity_id(log)]]) %>%
		summarize("absolute" = n_distinct(.data[[case_id(log)]])) %>%
		mutate("relative" = .data[["absolute"]] / n_cases(log))
}


