
#' @title Referral matrix repetitons
#'
#' @description Provides a list of initatiors and completers of  redo repetitons
#'
#'
#'
#'
#' @inherit activity_frequency params references seealso return
#' @seealso \code{\link{number_of_repetitions}}
#'
#' @export
#'
redo_repetitions_referral_matrix <- function(eventlog) {
	UseMethod("redo_repetitions_referral_matrix")
}

#' @describeIn redo_repetitions_referral_matrix Compute matrix for eventlog
#' @export

redo_repetitions_referral_matrix.eventlog <- function(eventlog) {
	first_resource <- NULL
	last_resource <- NULL

	eventlog %>%
		redo_repetitions() %>%
		group_by(first_resource, last_resource) %>%
		summarize(absolute = n()) -> output

	class(output) <- c("referral_matrix", class(output))
	attr(output, "type") <- "repetitions"
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}

