#' @title Trace coverage print
#'
#' @description  Print Trace coverage Information
#' @param x Data to print
#' @param ... Additional arguments
#' @method print trace_coverage

#' @export

print.trace_coverage <- function(x, ...) {
	data <- x

	if(attr(data, "level") == "log" & is.null(attr(data, "groups"))) {
		attr(data, "raw") <- NULL
		attr(data, "level") <- NULL
		attr(data, "mapping") <- NULL
		class(data) <- c("numeric")
		print.default(data)
	}
	else {
		print(tibble::trunc_mat(data))
	}
}
