#' @title Metric: Trace Frequency
#'
#' @description Computes the absolute and relative frequency of the traces in the event log.
#' #'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'
#' @export trace_frequency

trace_frequency <- function(eventlog) {
	library(dplyr)

	stop_eventlog(eventlog)


	tr <- traces(eventlog)

	tr <- tr %>% select(trace, absolute_frequency, relative_frequency) %>% arrange(desc(absolute_frequency))
	tr$cum_sum <- cumsum(tr$relative_frequency)
	colnames(tr) <- c("trace","absolute","relative", "cum_sum")
	return(tr)
}
