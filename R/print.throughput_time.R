#' @title Throughput Time Print
#'
#' @description  Print Throughput Time Information
#' @param x Data to print
#' @param ... Additional arguments
#' @method print throughput_time

#' @export

print.throughput_time <- function(x, ...) {
	data <- x

	if(attr(data, "level") == "log") {
		attr(data, "raw") <- NULL
		attr(data, "level") <- NULL
		attr(data, "mapping") <- NULL
		print.default(data)
	}
	else {
		print(tibble::trunc_mat(data))
	}
}
