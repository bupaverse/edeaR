


#' @title Set operations
#' @name setdiff
#' @param x Eventlog
#' @param y Eventlog
#'
#' @return Eventlog of activity instances in x which do not belong to y
#'
#' @importFrom dplyr setdiff
#' @export
dplyr::setdiff
#' @describeIn setdiff Setdiff for eventlogs
#' @export

setdiff.eventlog <- function(x, y, ...) {

	if(!("eventlog" %in% class(y)))
	   stop("Argument y should be event log")

	if (nrow(x) == nrow(y)) {
		complement <- x[c(),]
	}
	else if (nrow(y) == 0) {
		complement <- x
	}
	else {
		Identifier          <- activity_instance_id(x)
		x_act_id  <- pull(x[Identifier])
		y_act_id     <- pull(y[Identifier])
		Comp                <- which(x_act_id %in% y_act_id)
		complement          <- x[-Comp,]
	}
	complement %>%
		re_map(mapping(x))
}
