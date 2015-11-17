#' @title n_events
#'
#' @export n_events

n_events <- function(eventlog) {
	stop_eventlog(eventlog)
	return(nrow(eventlog))
}
