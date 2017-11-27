#' @title Referral matric selfloops
#'
#' @description Provides a list of initatiors and completers of  redo selfloops
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'
#'
#' @export redo_selfloops_referral_matrix
#'
redo_selfloops_referral_matrix <- function(eventlog) {
	UseMethod("redo_selfloops_referral_matrix")
}

#' @describeIn redo_selfloops_referral_matrix Compute matrix for eventlog
#' @export

redo_selfloops_referral_matrix.eventlog <- function(eventlog) {
	first_resource <- NULL
	last_resource <- NULL

	eventlog %>%
		redo_selfloops() %>%
		group_by(first_resource, last_resource) %>%
		summarize(absolute = n()) -> output

	class(output) <- c("referral_matrix", class(output))
	attr(output, "type") <- "selfloop"
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}

