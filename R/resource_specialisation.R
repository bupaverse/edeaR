#' @title Metric: Resource Specialisation
#'
#' @description Analyses whether resources specialise in specific activities
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level At which level the analysis of  coverage should be performed: log, case, resource.
#' @inheritParams resource_involvement
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

resource_specialisation.eventlog <- function(eventlog, level = c("log","case","activity","resource"), append = F, ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

	FUN <- switch(level,
				  log = resource_specialisation_log,
				  case = resource_specialisation_case,
				  activity = resource_specialisation_activity,
				  resource = resource_specialisation_resource)

	output <- FUN(eventlog = eventlog)


	return_metric(eventlog, output, level, append, "resource_specialisation", ifelse(level == "case",8,2))
}

#' @describeIn resource_specialisation Resource specialization for grouped eventlog
#' @export

resource_specialisation.grouped_eventlog <- function(eventlog, level = c("log","case","activity","resource"), append = F, ...) {

	level <- match.arg(level)
	level <- deprecated_level(level, ...)

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


	return_metric(eventlog, output, level, append, "resource_specialisation", ifelse(level == "case",8,2))
}


