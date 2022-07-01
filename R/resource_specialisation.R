#' @title Resource Specialisation
#'
#' @description Analyses whether resources specialise in specific activities.
#'
#' This metric can give an overview of which resources are performing certain activities more than others, and which resources
#' are responsible for containing all knowledge or capabilities on one topic.
#'
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"} (default),
#' \code{"case"}, \code{"activity"}, or \code{"resource"}. For more information, see \code{vignette("metrics", "edeaR")} and 'Details' below.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, this metric provides summary statistics on the number of distinct activities executed per resource.
#' \item On \code{"case"} level, this metric provides the number of distinct activities that are executed within each case
#' together with the summary statistics of the distinct activities executed per resource in each case.
#' \item On \code{"activity"} level, this metric provides an overview of the absolute and relative number of different resources
#' executing this activity within the complete log. This will give insights into which activities resources are specialised in.
#' \item On \code{"resource"} level, this metric shows the absolute and relative number of distinct activities that each resource executes.
#' }
#'
#' @inherit activity_frequency params references seealso return
#'
#' @family metrics
#'
#' @export resource_specialisation
resource_specialisation <- function(log,
									level = c("log", "case", "activity", "resource"),
									append = deprecated(),
									append_column = NULL,
									sort = TRUE,
									eventlog = deprecated()) {
	UseMethod("resource_specialisation")
}

#' @rdname resource_specialisation
#' @export resource_specialization
resource_specialization <- function(log,
									level = c("log", "case", "activity", "resource"),
									append = deprecated(),
									append_column = NULL,
									sort = TRUE,
									eventlog = deprecated()) {
	UseMethod("resource_specialisation")
}

#' @describeIn resource_specialisation Computes the resource specialisation for a \code{\link[bupaR]{log}}.
#' @export
resource_specialisation.log <- function(log,
										level = c("log", "case", "activity", "resource"),
										append = deprecated(),
										append_column = NULL,
										sort = TRUE,
										eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "resource_specialisation(eventlog)",
			with = "resource_specialisation(log)")
		log <- eventlog
	}
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "absolute",
								   level == "activity"~"absolute",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = resource_specialisation_log,
				  case = resource_specialisation_case,
				  activity = resource_specialisation_activity,
				  resource = resource_specialisation_resource)

	output <- FUN(log = log)

	if(sort && level %in% c("activity", "resource")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append,append_column, "resource_specialisation", ifelse(level == "case",10,2))
}

#' @describeIn resource_specialisation Computes the resource specialisation for a \code{\link[bupaR]{grouped_log}}.
#' @export
resource_specialisation.grouped_log <- function(log,
												level = c("log", "case", "activity", "resource"),
												append = deprecated(),
												append_column = NULL,
												sort = TRUE,
												eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)
	append <- lifecycle_warning_append(append)

	level <- rlang::arg_match(level)

	absolute <- NULL

	if(is.null(append_column)) {
		append_column <- case_when(level == "case" ~ "median",
								   level == "resource" ~ "absolute",
								   level == "activity"~"absolute",
								   T ~ "NA")
	}

	FUN <- switch(level,
				  log = resource_specialisation_log,
				  case = resource_specialisation_case,
				  activity = resource_specialisation_activity,
				  resource = resource_specialisation_resource)

	output <- bupaR:::apply_grouped_fun(log, FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	#if(!(level %in% c("log"))) {
	#	grouped_metric(eventlog, FUN) -> output
	#}
	#else {
	#	grouped_metric_raw_log(eventlog, FUN) -> output
	#}

	if(sort && level %in% c("activity", "resource")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(log, output, level, append,append_column, "resource_specialisation", ifelse(level == "case",8,2))
}


