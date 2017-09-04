#' @title Metric: Resource Specialisation
#'
#' @description Analyses whether resources specialise in specific activities
#'
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param level_of_analysis At which level the analysis of  coverage should be performed: log, case, resource.
#'
#' @export resource_specialisation


resource_specialisation <- function(eventlog, level_of_analysis = c("log","case","activity","resource")) {

	stop_eventlog(eventlog)
	level_of_analysis <- match.arg(level_of_analysis)
	mapping <- mapping(eventlog)

	FUN <- switch(level_of_analysis,
				  log = resource_specialisation_log,
				  case = resource_specialisation_case,
				  activity = resource_specialisation_activity,
				  resource = resource_specialisation_resource)


	if("grouped_eventlog" %in% class(eventlog)) {
		if(!(level_of_analysis %in% c("log"))) {
			eventlog %>%
				nest %>%
				mutate(data = map(data, re_map, mapping)) %>%
				mutate(data = map(data, FUN)) %>%
				unnest -> output
		}
		else {
			eventlog %>%
				nest %>%
				mutate(data = map(data, re_map, mapping)) %>%
				mutate(data = map(data, FUN)) -> temp

			temp %>%
				mutate(raw = map(data, attr, "raw")) %>%
				select(-data) %>%
				unnest() -> raw

			temp %>%
				mutate(data = map(data, ~as.data.frame(as.list(.x)))) %>%
				unnest() -> output

			attr(output, "raw") <- raw
		}

		attr(output, "groups") <- groups(eventlog)
	}
	else{
		output <- FUN(eventlog = eventlog)
	}


	class(output) <- c("resource_specialisation", class(output))
	attr(output, "level") <- level_of_analysis
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}

#' @rdname resource_specialisation
#' @export resource_specialization

resource_specialization <- function(eventlog, level_of_analysis = c("log","case","resource","activity")) {
	eventlog %>%
		resource_specialisation(level_of_analysis = level_of_analysis) %>%
		return()
}
