#' @title Filter: Case
#'
#' @description Filters the log based on case identifier
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param cases A vector of cases to withhold
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_case
#'
filter_case <- function(eventlog,
							cases = NULL,
							reverse = F){
	stop_eventlog(eventlog)
	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"


	if(reverse == F)
		output <- filter(eventlog, case_classifier %in% cases)

	else
		output <- filter(eventlog, !(case_classifier %in% cases))

	colnames(output)[colnames(output)=="case_classifier"] <- case_id(eventlog)

	output <- re_map(output, mapping(eventlog))

	return(output)
}
