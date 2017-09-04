#' @title Idle Time Print
#'
#' @description  Print idle time Information
#' @param x Data to print
#' @param ... Additional arguments
#' @method print idle_time

#' @export

print.idle_time <- function(x, ...) {
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
