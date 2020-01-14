#' Filter: Start and end conditions
#'
#' Filters cases where the first and/or last activity adhere to the specified conditions
#'
#'The filter_endpoints method filters cases based on the first and last activity label. It can be used in two ways: by specifying vectors with allowed start
#'activities and/or allowed end activities, or by specifying a percentile. In the latter case, the percentile value will be used as a cut off.
#'For example, when set to 0.9, it will select the most common endpoint pairs which together cover at least 90% of the cases, and filter the event log accordingly.
#'
#'
#' @param start_condition A logical condition
#'
#' @param end_condition A logical condition
#'
#'
#' @inherit filter_activity params references seealso return
#'
#' @export filter_endpoints_conditions

filter_endpoints_conditions <- function(eventlog,
							 start_condition,
							 end_condition,
							 reverse,
							 ...) {
	UseMethod("filter_endpoints_conditions")
}

#' @export

filter_endpoints_conditions.eventlog <- function(eventlog,
									  start_condition = NULL,
									  end_condition = NULL,
									  reverse = FALSE,
									  ...) {

	start_condition_specified <- FALSE
	tryCatch({
		is.null(start_condition)
	}, error = function(e) {
		start_condition_specified <<- TRUE
	}
	)

	if(!start_condition_specified) {
		# geen filter gespecifieerd.
		eventlog <- eventlog %>%
			mutate(START_CONDITION = TRUE)
	} else {
		start_condition <- enquo(start_condition)
		error_cond <- FALSE

		tryCatch({
			eventlog <- eventlog %>% mutate(START_CONDITION = !!(start_condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The start condition (", expr_text(start_condition), ") is not valid. Check the syntax and column names.")
		}
	}
	end_condition_specified <- FALSE
	tryCatch({
		is.null(end_condition)
	}, error = function(e) {
		end_condition_specified <<- TRUE
	}
	)

	if(!end_condition_specified) {
		# geen filter gespecifieerd.
		eventlog <- eventlog %>%
			mutate(END_CONDITION = TRUE)
	} else {
		end_condition <- enquo(end_condition)
		error_cond <- FALSE

		tryCatch({
			eventlog <- eventlog %>% mutate(END_CONDITION = !!(end_condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The end condition (", expr_text(end_condition), ") is not valid. Check the syntax and column names.")
		}
	}

	eventlog %>%
		group_by_case() %>%
		arrange(!!timestamp_(eventlog), .order) %>%
		summarize(START_CONDITION = first(START_CONDITION),
				  END_CONDITION = last(END_CONDITION)) %>%
		filter(START_CONDITION & END_CONDITION) %>%
		pull(!!case_id_(eventlog)) %>%
		unique() -> case_selection

	eventlog %>%
		select(-START_CONDITION, -END_CONDITION) %>%
		filter_case(cases = case_selection, reverse = reverse)

}
#' @export

filter_endpoints_conditions.grouped_eventlog <- function(eventlog,
											  start_condition = NULL,
											  end_condition = NULL,
											  reverse = FALSE,
											  ...) {
	grouped_filter(eventlog, filter_endpoints_conditions.grouped_eventlog, start_condition, end_condition, reverse, ...)
}





