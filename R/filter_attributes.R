#' @title Generic filter function for eventlog
#' @description Generic filter function for eventlog
#' @param eventlog  Eventlog object
#' @param ... Filter conditions
#' @export
filter_attributes <- function(eventlog, ...) {
	UseMethod("filter_attributes")
}

#' @describeIn filter_attributes Filter eventlog using attributes
#' @export

filter_attributes.eventlog <- function(eventlog, ...) {

	mapping <- mapping(eventlog)
	eventlog %>%
		as.data.frame() %>%
		dplyr::filter(...) %>%
		re_map(mapping) %>%
		return()

}

#' @describeIn filter_attributes Filter grouped eventlog using attributes
#' @export

filter_attributes.grouped_eventlog <- function(eventlog, ...) {
	mapping <- mapping(eventlog)
	eventlog %>%
		as.data.frame() %>%
		dplyr::filter(...) %>%
		re_map(mapping) %>%
		group_by_at(vars(one_of(paste(groups(eventlog)))))

}

#' @rdname filter_attributes
#' @importFrom dplyr filter
#' @export
dplyr::filter

#' @describeIn filter_attributes Filter eventlog
#' @export
filter.eventlog <- function(.data, ...) {
	filter_attributes(.data, ...)
}
