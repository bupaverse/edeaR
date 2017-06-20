#' @title Generic filter function for eventlog
#' @description Generic filter function for eventlog
#' @param eventlog  Eventlog object
#' @param ... Filter conditions
#' @export

filter_attributes <- function(eventlog, ...) {

	stop_eventlog(eventlog)

	mapping <- mapping (eventlog)

	dplyr::filter(eventlog, ...) %>%
		re_map(mapping) %>%
		return()

}

