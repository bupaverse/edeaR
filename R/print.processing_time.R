#' @title Processing Time Print
#'
#' @description  Print Processing Time Information
#' @param x Data to print
#' @param ... Additional arguments
#' @method print processing_time

#' @export

print.processing_time <- function(x, ...) {
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
