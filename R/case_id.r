#' @title Case classifier
#' 
#' @description Get the case classifier of an object of class \code{eventlog} 
#' 
#' @param eventlog An object of class \code{eventlog}. 
#'
#' @seealso \code{\link{eventlog}}, \code{\link{activity_id}},
#' \code{\link{start_timestamp}}, \code{\link{complete_timestamp}} 
#' @examples
#' 
#' 
#' data(example_log)
#' case_id(example_log)
#' 
#' @export case_id
#' 
case_id <- function(eventlog){
	if("eventlog" %in% class(eventlog))
		return(attr(eventlog, "case_id"))
	else
		stop("Function only applicable on objects of type 'eventlog'")
}
