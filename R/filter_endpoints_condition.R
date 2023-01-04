#' @title Filter Start and End Conditions
#'
#' @description Filters cases where the first and/or last activity adhere to the specified conditions.
#'
#' @param start_condition,end_condition A logical condition.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_endpoints_condition
filter_endpoints_condition <- function(log,
									   start_condition = NULL,
									   end_condition = NULL,
									   reverse = FALSE,
									   eventlog = deprecated()) {
	UseMethod("filter_endpoints_condition")
}

#' @describeIn filter_endpoints_condition Filters cases for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_endpoints_condition.eventlog <- function(log,
												start_condition = NULL,
												end_condition = NULL,
												reverse = FALSE,
												eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_endpoints_condition(eventlog)",
			with = "filter_endpoints_condition(log)")
		log <- eventlog
	}

	START_CONDITION <- NULL
	END_CONDITION <- NULL

	start_condition_specified <- FALSE
	tryCatch({
		start_condition_specified <- !is.null(start_condition)
	}, error = function(e) {
		start_condition_specified <<- TRUE
	})
	if(!start_condition_specified) {
		# geen filter gespecifieerd.
		log <- log %>%
			mutate(START_CONDITION = TRUE)
	} else {
		start_condition <- enquo(start_condition)
		error_cond <- FALSE

		tryCatch({
			log <- log %>% mutate(START_CONDITION = !!(start_condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The start condition (", expr_text(start_condition), ") is not valid. Check the syntax and column names.")
		}
	}
	end_condition_specified <- FALSE
	tryCatch({
		end_condition_specified <- is.null(end_condition)
	}, error = function(e) {
		end_condition_specified <<- TRUE
	})

	if(!end_condition_specified) {
		# geen filter gespecifieerd.
		log <- log %>%
			mutate(END_CONDITION = TRUE)
	} else {
		end_condition <- enquo(end_condition)
		error_cond <- FALSE

		tryCatch({
			log <- log %>% mutate(END_CONDITION = !!(end_condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The end condition (", expr_text(end_condition), ") is not valid. Check the syntax and column names.")
		}
	}


	log %>%
		group_by_case() %>%
		arrange(!!timestamp_(log), .order) %>%
		summarize(START_CONDITION = first(START_CONDITION),
				  END_CONDITION = last(END_CONDITION))	%>%
		filter(START_CONDITION & END_CONDITION) %>%
		pull(!!case_id_(log)) %>%
		unique() -> case_selection

	log %>%
		select(-START_CONDITION, -END_CONDITION) %>%
		filter_case(cases = case_selection, reverse = reverse)
}

#' @describeIn filter_endpoints_condition Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_endpoints_condition.grouped_log <- function(log,
												   start_condition = NULL,
												   end_condition = NULL,
												   reverse = FALSE,
												   eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_endpoints_condition(eventlog)",
			with = "filter_endpoints_condition(log)")
		log <- eventlog
	}

	bupaR:::apply_grouped_fun(log, fun = filter_endpoints_condition, enquo(start_condition), enquo(end_condition), reverse,
							  .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_endpoints_conditions.grouped_eventlog, start_condition, end_condition, reverse, ...)
}

#' @describeIn filter_endpoints_condition Filters cases for an \code{\link[bupaR]{activitylog}}.
#' @export
filter_endpoints_condition.activitylog <- function(log,
												   start_condition = NULL,
												   end_condition = NULL,
												   reverse = FALSE,
												   eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_endpoints_condition(eventlog)",
			with = "filter_endpoints_condition(log)")
		log <- eventlog
	}

	filter_endpoints_condition(log = bupaR::to_eventlog(log), start_condition = enquo(start_condition), end_condition = enquo(end_condition), reverse = reverse) %>%
		bupaR::to_activitylog()
}

#' @rdname filter_endpoints_condition
#' @export
#'
filter_endpoints_conditions <- function(log,
										start_condition = NULL,
										end_condition = NULL,
										reverse = FALSE,
										eventlog = deprecated()) {
	lifecycle::deprecate_warn(
		"0.9.0",
		what = "filter_endpoints_conditions()",
		with = "filter_endpoints_condition()"
	)

	filter_endpoints_condition(log,
							   enquo(start_condition),
							   enquo(end_condition),
							   reverse)

}



