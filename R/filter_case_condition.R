#' @title Filter Case Condition
#'
#' @description Filters cases using a condition. Only keeps cases if the condition is valid for at least one event.
#'
#' @param ... [`data-masking`][dplyr_data_masking]: Expressions that return a logical value, and are defined in terms of the variables in \code{log}.
#' If multiple expressions are included, they are combined with the \code{&} operator. Only rows for which all conditions evaluate to \code{TRUE} are kept.
#' For more information, see \code{\link[dplyr]{filter}}.
#' @param condition `r lifecycle::badge("deprecated")`; please use [`data-masking`][dplyr_data_masking] expressions instead.
#'
#' @inherit filter_activity params references seealso return
#'
#' @seealso \code{\link[dplyr]{filter}}
#'
#' @family filters
#'
#' @export filter_case_condition
filter_case_condition <- function(log, ..., condition = NULL, reverse = FALSE, eventlog = deprecated()) {
	UseMethod("filter_case_condition")
}

#' @describeIn filter_case_condition Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_case_condition.log <- function(log, ..., condition = deprecated(), reverse = FALSE, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_case_condition(eventlog)",
			with = "filter_case_condition(log)")
		log <- eventlog
	}


	condition_specified <- FALSE
	tryCatch({
		is.null(condition)
	}, error = function(e) {
		condition_specified <<- TRUE
	})

	if(!condition_specified) {
		# condition arg not specified, fallback to ... args (does not necessarily have to be present)
		cases <- log %>% filter(...) %>% case_labels()
	} else {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_case_condition(condition)",
			with = "filter_case_condition(...)")

		condition <- rlang::enquo(condition)
		error_cond <- FALSE

		tryCatch({
			eventlog_filtered <- log %>% filter(!!(condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The condition (", rlang::expr_text(condition), ") is not valid. Check the syntax and column names.")
		}

		cases <- eventlog_filtered %>% case_labels()
	}

	filter_case.log(log = log, cases = cases, reverse = reverse)
}

#' @describeIn filter_case_condition Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_case_condition.grouped_log <- function(log, ..., condition = deprecated(), reverse = FALSE, eventlog = deprecated()) {
	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_case_condition(eventlog)",
			with = "filter_case_condition(log)")
		log <- eventlog
	}

	bupaR:::apply_grouped_fun(log, fun = filter_case_condition.log, ..., reverse = reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

