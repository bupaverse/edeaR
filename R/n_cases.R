#' @title n_cases
#'
#' @export n_cases

n_cases <- function(eventlog) {

	stop_eventlog(eventlog)

	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"

	return(length(unique(eventlog$case_classifier)))
}
