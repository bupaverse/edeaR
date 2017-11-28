#'  Metric: Number of traces
#'
#' Computes how many traces there are.
#'
#' This metric provides two values, the absolute and relative number of traces that
#' occur in the event log. The relative number shows expected number of traces needed to cover 100 cases.
#'

#' @inherit activity_frequency params references seealso return
#'
#'
#' @export number_of_traces
#'
number_of_traces <- function(eventlog) {
	UseMethod("number_of_traces")
}

#' @describeIn number_of_traces Number of traces in eventlog
#' @export


number_of_traces.eventlog <- function(eventlog) {
	FUN <- number_of_traces_FUN
	output <- FUN(eventlog = eventlog)
	class(output) <- c("number_of_traces", class(output))
	attr(output, "mapping") <- mapping(eventlog)
	return(output)
}

#' @describeIn number_of_traces Number of traces in each group of eventlog
#' @export

number_of_traces.grouped_eventlog <- function(eventlog) {
	FUN <- number_of_traces_FUN

	grouped_metric(eventlog, FUN) -> output

	class(output) <- c("number_of_traces", class(output))
	attr(output, "mapping") <- mapping(eventlog)
	return(output)
}



number_of_traces_FUN <- function(eventlog) {

	tr <- traces(eventlog)

	r<- data.frame( c(nrow(tr),sum(tr$absolute_frequency)/nrow(tr)))
	r <- as.data.frame(t(r))
	colnames(r) <- c("absolute","average_coverage")
	r <- tbl_df(r)
	return(r)
}
