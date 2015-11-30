#' @title n_traces
#'
#' @export n_traces



n_traces <- function(eventlog) {
	stop_eventlog(eventlog)
	return(nrow(traces(eventlog)))
}
