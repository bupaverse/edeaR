#' @title Filter Case Condition
#'
#' @description Filters cases using a condition. Only keeps cases if the condition is valid for at least one event.
#'
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in \code{log}.
#' If multiple expressions are included, they are combined with the \code{&} operator. Only rows for which all conditions evaluate to \code{TRUE} are kept.
#' For more information, see \code{\link[dplyr]{filter}}.
#'
#' @inherit filter_activity params references seealso return
#'
#' @seealso \code{\link[dplyr]{filter}}
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_case_condition
filter_case_condition <- function(log, ..., reverse = FALSE) {
	UseMethod("filter_case_condition")
}

#' @describeIn filter_case_condition Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_case_condition.log <- function(log, ..., reverse = FALSE) {


	cases <- log %>% filter(...) %>% case_labels()

	filter_case.log(log = log, cases = cases, reverse = reverse)
}

#' @describeIn filter_case_condition Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_case_condition.grouped_log <- function(log, ..., reverse = FALSE) {

	bupaR:::apply_grouped_fun(log, fun = filter_case_condition.log, ..., reverse = reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

