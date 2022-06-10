#' title Filter: Case
#'
#' Filters cases using a condition
#'
#' Only keeps cases if the condition is valid for at least one event.
#'
#' @param condition A condition
#'
#' @inherit filter_activity params references seealso return
#' @export filter_case_condition

filter_case_condition <- function(eventlog,
								  condition = NULL,
								  reverse = FALSE) {
	UseMethod("filter_case_condition")
}


#' @export
filter_case_condition.eventlog <- function(eventlog,
										   condition = NULL,
								 reverse = FALSE){


	condition_specified <- FALSE
	tryCatch({
		is.null(condition)
	}, error = function(e) {
		condition_specified <<- TRUE
	}
	)

	if(!condition_specified) {
		# geen filter gespecifieerd.
		stop("No condition specified.")
	} else {
		condition <- enquo(condition)
		error_cond <- FALSE

		tryCatch({
			eventlog_filtered <- eventlog %>% filter(!!(condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The condition (", expr_text(condition), ") is not valid. Check the syntax and column names.")
		}
	}
	cases <- eventlog_filtered %>% case_labels

	if(!reverse)
		filter(eventlog, (!!as.symbol(case_id(eventlog))) %in% cases)
	else
		filter(eventlog, !((!!as.symbol(case_id(eventlog))) %in% cases))
}

#' @export

filter_case_condition.grouped_eventlog <- function(eventlog, condition = NULL, reverse = FALSE) {
	grouped_filter(eventlog, filter_case_condition, condition, reverse)
}

