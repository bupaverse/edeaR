#' @title Resource Specialisation
#'
#' @description Analyses whether resources specialise in specific activities.
#'
#' This metric can give an overview of which resources are performing certain activities more than others, and which resources
#' are responsible for containing all knowledge or capabilities on one topic.
#'
#' @param level \code{\link{character}} (default \code{"log"}): Level of granularity for the analysis: \code{"log"} (default),
#' , \code{"activity"}, or \code{"resource"}. For more information, see \code{vignette("metrics", "edeaR")} and 'Details' below.
#'
#' @details
#' Argument \code{level} has the following options:
#' \itemize{
#' \item At \code{"log"} level, this metric provides summary statistics on the number of distinct activities executed per resource.
#' \item On \code{"activity"} level, this metric provides an overview of the absolute and relative number of different resources
#' executing this activity within the complete log. This will give insights into which activities resources are specialised in.
#' \item On \code{"resource"} level, this metric shows the absolute and relative number of distinct activities that each resource executes.
#' }
#'
#' @inherit activity_frequency params references seealso return
#'
#' @family metrics
#'
#' @concept metrics_organizational
#'
#' @export resource_specialisation
resource_specialisation <- function(log,
									level = c("log", "activity", "resource"),
									sort = TRUE) {
	UseMethod("resource_specialisation")
}

#' @rdname resource_specialisation
#' @export resource_specialization
resource_specialization <- function(log,
									level = c("log", "activity", "resource"),
									sort = TRUE) {
	UseMethod("resource_specialisation")
}

#' @describeIn resource_specialisation Computes the resource specialisation for a \code{\link[bupaR]{log}}.
#' @export
resource_specialisation.log <- function(log,
										level = c("log", "activity", "resource"),
										sort = TRUE) {


	level <- rlang::arg_match(level)

	absolute <- NULL


	FUN <- switch(level,
				  log = resource_specialisation_log,
				  activity = resource_specialisation_activity,
				  resource = resource_specialisation_resource)

	output <- FUN(log = log)

	if(sort && level %in% c("activity", "resource")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric_v2(log, output, level, "resource_specialisation")
}

#' @describeIn resource_specialisation Computes the resource specialisation for a \code{\link[bupaR]{grouped_log}}.
#' @export
resource_specialisation.grouped_log <- function(log,
												level = c("log", "activity", "resource"),
												sort = TRUE) {

	level <- rlang::arg_match(level)

	absolute <- NULL

	FUN <- switch(level,
				  log = resource_specialisation_log,
				  activity = resource_specialisation_activity,
				  resource = resource_specialisation_resource)

	output <- apply_grouped_fun(log, FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

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

	return_metric_v2(log, output, level,"resource_specialisation")
}


