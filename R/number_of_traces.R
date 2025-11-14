#' @title Number of Traces
#'
#' @description Computes how many traces there are.
#'
#' This metric provides two values, the absolute and relative number of traces that occur in the log.
#' The relative number shows expected number of traces needed to cover 100 cases.
#'
#' @inherit activity_frequency params references seealso return
#'
#' @seealso \code{\link[bupaR]{traces}}
#'
#' @family metrics
#'
#' @concept metrics_structuredness
#'
#' @export number_of_traces
number_of_traces <- function(log) {
	UseMethod("number_of_traces")
}

#' @describeIn number_of_traces Number of traces in a \code{\link[bupaR]{log}}.
#' @export
number_of_traces.log <- function(log) {

	output <- number_of_traces_FUN(log)
	class(output) <- c("number_of_traces", class(output))
	attr(output, "mapping") <- mapping(log)
	return(output)
}

#' @describeIn number_of_traces Number of traces in a \code{\link[bupaR]{grouped_log}}.
#' @export
number_of_traces.grouped_log <- function(log) {

	output <- bupaR:::apply_grouped_fun(log, fun = number_of_traces_FUN, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)

	class(output) <- c("number_of_traces", class(output))
	attr(output, "mapping") <- mapping(log)
	return(output)
}


number_of_traces_FUN <- function(log) {

	tr <- bupaR::traces(log)

	r <- data.frame(c(nrow(tr), sum(tr$absolute_frequency)/nrow(tr)))
	r <- as.data.frame(t(r))
	colnames(r) <- c("absolute", "average_coverage")
	r <- as_tibble(r)

	return(r)
}
