#' Metric: Resource Involvement
#'
#' Calculates for each resource/resource-activity in what percentage of cases it is present.
#'
#
#'Next to the resource frequency, the involvement of resources in cases can be of interest to, e.g., decide how "indispensable"
#' they are. This metric is provided on three levels of analysis, which are the cases,
#' the resources, and the resource-activity combinations
#'
#' \itemize{
#'
#' \item At the level of the specific
#' cases, the absolute and relative number of distinct resources executing activities
#' in each case is calculated. This way a company gets an overview of which
#' cases are handled by a small amount of resources and which cases need more
#' resources, indicating a higher level of variance in the process.
#'
#' \item On the level of the distinct resources,
#' this metric provides the absolute and relative number of cases in which each
#' resource is involved, indicating which resources are more "necessary" within the
#' business process than the others.
#'
#' \item On the level of the specific
#' resource-activity combinations, this metric provides a list of all resource-activity
#' combinations with the absolute and relative number of cases in which each resource-activity combination is involved.
#'
#'
#' }
#'
#' @param level Level of granularity for the analysis: log,  case, activity, resource or resource-activity.
#' For more information, see \code{vignette("metrics", "edeaR")}
#' @inherit activity_frequency params references seealso return
#'
#' @export resource_involvement

resource_involvement <- function(eventlog, level, append, ...) {
	UseMethod("resource_involvement")
}

#' @describeIn resource_involvement Resource involvement for eventlog
#' @export

resource_involvement.eventlog <- function(eventlog,
										  level = c("case","resource","resource-activity"),
										  append = F,
										  append_column = NULL,
										  sort = TRUE,
										  ...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   T ~ "NA")
	}


	FUN <- switch(level,
				  case = resource_involvement_case,
				  resource = resource_involvement_resource,
				  "resource-activity" = resource_involvement_resource_activity)

	output <- FUN(eventlog = eventlog)
	if(sort) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append, append_column, "resource_involvement",2)
}

#' @describeIn resource_involvement Resource involvement for grouped eventlog
#' @export

resource_involvement.grouped_eventlog <- function(eventlog,
												  level = c("case","resource","resource-activity"),
												  append = F,
												  append_column = NULL,
												  sort = TRUE,
												   ...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "absolute",
								   level == "resource" ~ "absolute",
								   level == "resource-activity"~"absolute",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  case = resource_involvement_case,
				  resource = resource_involvement_resource,
				  "resource-activity" = resource_involvement_resource_activity)

	output <- grouped_metric(eventlog, FUN)
	if(sort) {
		output %>%
			arrange(-absolute) -> output
	}
	return_metric(eventlog, output, level, append, append_column, "resource_involvement",2)
}
