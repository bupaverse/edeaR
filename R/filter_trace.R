

#' title Filter: Trace
#'
#' Filters the log based on trace id
#'
#' The method filter_trace can be used to filter on trace id It has an trace_ids argument,
#' to which a vector of identifiers can be given. The selection can be negated with the reverse argument.
#'
#' @param trace_ids A vector of trace identifiers
#'
#' @inherit filter_activity params references seealso return
#' @export filter_trace

filter_trace <- function(eventlog, trace_ids, reverse) {
	UseMethod("filter_trace")
}


#' @export
filter_trace.eventlog <- function(eventlog,
								  trace_ids = NULL,
								 reverse = FALSE){
	trace_id <- NULL

	eventlog %>%
		case_list %>%
		filter(trace_id %in% trace_ids) %>%
		pull(1) -> cases

	if(!reverse)
		filter(eventlog, (!!as.symbol(case_id(eventlog))) %in% cases)
	else
		filter(eventlog, !((!!as.symbol(case_id(eventlog))) %in% cases))
}

#' @export

filter_trace.grouped_eventlog <- function(eventlog, trace_ids = NULL, reverse = FALSE) {
	grouped_filter(eventlog, filter_trace, trace_ids, reverse)
}


