#' @title Referral matric repetitons
#'
#' @description Provides a list of initatiors and completers of  redo repetitons
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'
#'
#' @export redo_repetitions_referral_matrix

redo_repetitions_referral_matrix <- function(eventlog) {
	eventlog %>%
		redo_repetitions() %>%
		group_by(first_resource, last_resource) %>%
		summarize(absolute = n()) -> output

	class(output) <- c("referral_matrix", class(output))
	attr(output, "type") <- "repetitions"
	attr(output, "mapping") <- mapping(eventlog)

	return(output)
}

