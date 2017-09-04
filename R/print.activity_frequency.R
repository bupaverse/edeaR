#' @title Activity Frequency Print
#'
#' @description  Print Actvitity Frequency Information
#' @param x Data to print
#' @param ... Additional arguments
#' @method print activity_frequency

#' @export

print.activity_frequency <- function(x, ...) {
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
