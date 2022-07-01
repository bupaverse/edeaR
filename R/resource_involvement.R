#' @title Resource Involvement
#'
#' @description Calculates for each resource or resource-activity combination in what percentage of cases it is present.
#'
#' Next to the \code{\link{resource_frequency}}, the involvement of resources in cases can be of interest to, e.g.,
#' decide how "indispensable" they are. This metric is provided on three levels of analysis, which are the cases,
#' the resources, and the resource-activity combinations.
#'
#' @param level \code{\link{character}} (default \code{"case"}): Level of granularity for the analysis: \code{"case"},
#' \code{"resource"}, or \code{"resource-activity"}. For more information, see \code{vignette("metrics", "edeaR")} and 'Details' below.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item On \code{"case"} level, the absolute and relative number of distinct resources executing activities in each case
#' is calculated, to get an overview of which cases are handled by a small amount of resources and which cases need more
#' resources, indicating a higher level of variance in the process.
#' \item On \code{"resource"} level, this metric provides the absolute and relative number of cases in which each resource
#' is involved, indicating which resources are more "necessary" within the process than the others.
#' \item On \code{"resource-activity"} level, this metric provides a list of all resource-activity combinations with the
#' absolute and relative number of cases in which each resource-activity combination is involved.
#' }
#'
#' @inherit end_activities params
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link{resource_frequency}}
#'
#' @family metrics
#'
#' @export resource_involvement
resource_involvement <- function(log,
								 level = c("case", "resource", "resource-activity"),
								 append = deprecated(),
								 append_column = NULL,
								 sort = TRUE,
								 eventlog = deprecated()) {
	UseMethod("resource_involvement")
}

#' @describeIn resource_involvement Computes the resource involvement for a \code{\link[bupaR]{log}}.
#' @export
resource_involvement.log <- function(log,
									 level = c("case", "resource", "resource-activity"),
									 append = deprecated(),
									 append_column = NULL,
									 sort = TRUE,
									 eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "resource_involvement(eventlog)",
			with = "resource_involvement(log)")
		log <- eventlog
	}
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

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

	output <- FUN(log = log)

	if(sort) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "resource_involvement",2)
}

#' @describeIn resource_involvement Computes the resource involvement for a \code{\link[bupaR]{grouped_log}}.
#' @export
resource_involvement.grouped_log <- function(log,
											 level = c("case", "resource", "resource-activity"),
											 append = deprecated(),
											 append_column = NULL,
											 sort = TRUE,
											 eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

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

	output <- bupaR:::apply_grouped_fun(log, FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	if(sort) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append, append_column, "resource_involvement",2)
}
