#' @title Metric: Activity Presence
#'
#' @description Calculates for each activity type in what percentage of cases it is present.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
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
#'activity_presence(log)
#' }
#' @export activity_presence


activity_presence <- function(eventlog) {
	stop_eventlog(eventlog)

	FUN <- function(eventlog) {
		eventlog %>%
			group_by(!!as.symbol(activity_id(eventlog))) %>%
			summarize(absolute = n_distinct(!!as.symbol(case_id(eventlog)))) %>%
			mutate(relative = absolute/n_cases(eventlog)) %>%
			arrange(-absolute) -> output
		return(output)
	}

	mapping <- mapping(eventlog)

	if("grouped_eventlog" %in% class(eventlog)) {
			eventlog %>%
				nest %>%
				mutate(data = map(data, re_map, mapping)) %>%
				mutate(data = map(data, FUN)) %>%
				unnest -> output
		attr(output, "groups") <- groups(eventlog)
	}
	else{
		output <- FUN(eventlog = eventlog)
	}


class(output) <- c("activity_presence", class(output))
attr(output, "mapping") <- mapping(eventlog)
return(output)
}
