#' @title Filter Trace
#'
#' @description Filters the log based on trace identifier.
#'
#' This method can be used to filter on trace identifier, which can be obtained from \code{\link[bupaR]{case_list}}.
#' It has a \code{trace_ids} argument, to which a vector of identifiers can be given. The selection can be negated with the \code{reverse} argument.
#'
#' @param trace_ids \code{\link{character}} vector: A vector of trace identifiers
#'
#' @inherit filter_activity params references seealso return
#'
#' @seealso \code{\link[bupaR]{case_list}}
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_trace
filter_trace <- function(log, trace_ids, reverse = FALSE, eventlog = deprecated()) {
	UseMethod("filter_trace")
}

#' @describeIn filter_trace Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_trace.log <- function(log, trace_ids, reverse = FALSE, eventlog = deprecated()){

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_trace(eventlog)",
			with = "filter_trace(log)")
		log <- eventlog
	}

	trace_id <- NULL

	log %>%
		case_list() %>%
		filter(trace_id %in% trace_ids) %>%
		pull(1) -> cases

	filter_case.log(log, cases = cases, reverse = reverse)
}

#' @describeIn filter_trace Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_trace.grouped_log <- function(log, trace_ids, reverse = FALSE, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_trace(eventlog)",
			with = "filter_trace(log)")
		log <- eventlog
	}

	bupaR:::apply_grouped_fun(log, fun = filter_trace.log, trace_ids, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}
