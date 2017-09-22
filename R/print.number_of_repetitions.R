#' @title Repetitions Print
#'
#' @description  Print Repetitions Information
#' @param x Data to print
#' @param ... Additional arguments
#' @method print number_of_repetitions

#' @export

print.number_of_repetitions <- function(x, ...) {
	data <- x

	if(attr(data, "level") == "log" & is.null(attr(data, "groups"))) {
		attr(data, "raw") <- NULL
		attr(data, "level") <- NULL
		attr(data, "mapping") <- NULL
		class(data) <- "numeric"
		print.default(data)
	}
	else {
		print(tibble::trunc_mat(data))
	}
}
