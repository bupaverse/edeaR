#' @title Filter: Resource
#'
#' @description Filters the log based on resources
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param resources A vector of resources to withhold
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_resource
#'
filter_resource <- function(eventlog,
							resources = NULL,
							reverse = F){
	stop_eventlog(eventlog)
	colnames(eventlog)[colnames(eventlog) == resource_id(eventlog)] <- "resource_classifier"


	if(reverse == F)
		output <- filter(eventlog, resource_classifier %in% resources)

	else
		output <- filter(eventlog, !(resource_classifier %in% resources))

	colnames(output)[colnames(output)=="resource_classifier"] <- resource_id(eventlog)

	output <- re_map(output, mapping(eventlog))

	return(output)
}
