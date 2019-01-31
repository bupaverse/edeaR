#' Metric: Resource Specialisation
#'
#' Analyses whether resources specialise in specific activities
#'
#' This can give a
#' company an overview of which resources are performing certain activities more
#' than others, and which resources are responsible for containing all knowledge
#' or capabilities on one topic.
#'
#' \itemize{
#'
#' \item On the level of the complete event log,
#' this metric provides summary statistics on the number of distinct activities executed per
#' resource.
#'
#' \item On the level of the cases, this metric provides
#' the number of distinct activities that are executed within each case together
#' with the summary statistics of the distinct activities executed per resource
#' in each case.
#'
#' \item On the level of the distinct activities,
#' this metric provides an overview of the absolute and relative number of different
#' resources executing this activity within the complete event log. This will give a
#' company insights in which activities resources are specialised in.
#'
#' \item Finally, the resource specialisation can
#' also be calculated on the resource level, showing the absolute and relative number
#' of distinct activities that each resource executes.
#' }
#'
#' @param level Level of granularity for the analysis: log,  case, or resource.
#' For more information, see \code{vignette("metrics", "edeaR")}#'
#'
#' @inherit activity_frequency params references seealso return
#'
#' @export resource_specialisation

resource_specialisation <- function(eventlog, level, append, ...) {
	UseMethod("resource_specialisation")
}

#' @rdname resource_specialisation
#' @export resource_specialization

resource_specialization <- function(eventlog, level, append, ...) {
	UseMethod("resource_specialisation")
}


#' @describeIn resource_specialisation Resource specialization for  eventlog
#' @export

resource_specialisation.eventlog <- function(eventlog,
											 level = c("log","case","activity","resource"),
											 append = F,
											 append_column = NULL,
											 sort = TRUE,
											 ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)
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

	output <- FUN(eventlog = eventlog)
	if(sort && level %in% c("activity","resource")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(eventlog, output, level, append,append_column, "resource_specialisation", ifelse(level == "case",10,2))
}

#' @describeIn resource_specialisation Resource specialization for grouped eventlog
#' @export

resource_specialisation.grouped_eventlog <- function(eventlog,
													 level = c("log","case","activity","resource"),
													 append = F,
													 append_column = NULL,
													 sort = TRUE,
													 ...) {
	absolute <- NULL
	level <- match.arg(level)
	level <- deprecated_level(level, ...)

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


	if(!(level %in% c("log"))) {
		grouped_metric(eventlog, FUN) -> output
	}
	else {
		grouped_metric_raw_log(eventlog, FUN) -> output
	}

	if(sort && level %in% c("activity","resource")) {
		output %>%
			arrange(-absolute) -> output
	}

	return_metric(eventlog, output, level, append,append_column, "resource_specialisation", ifelse(level == "case",8,2))
}


