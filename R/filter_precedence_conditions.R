#' Filter: precedence relations
#'
#' Filters cases based on the precedence relations between two sets of activities.
#'
#' In order to extract a subset of an event log which conforms with a set of precedence rules, one can use the filter_precedence method. There are two types of
#' precendence relations which can be tested: activities that should directly follow each other,
#' or activities that should eventually follow each other. The type can be set with the precedence type argument.
#' Further, the filter requires a vector of one or more antecedents (containing activity labels), and one or more consequents. Finally, also a filter method argument
#' can be set. This argument is relevant when there is more than one antecedent or consequent.
#' In such a case, you can specify that all possible precedence combinations must be present (all), at least one of them (one of), or none (none).
#'
#' @param antecedent_condition,consequent_condition The antecendent and consequent conditions
#'
#' @param precedence_type When \code{directly_follows}, the consequent condition should hold immediately after the antecedent condition hold
#' When \code{eventually_follows}, other events are allowed to happen in between.
#'
#'
#' @inherit filter_activity params references seealso return
#' @export filter_precedence_condition

filter_precedence_condition <- function(eventlog,
							  antecedent_condition,
							  consequent_condition,
							  precedence_type,
							  reverse) {
	UseMethod("filter_precedence_condition")
}

#' @export

filter_precedence_condition.eventlog <- function(eventlog,
									   antecedent_condition,
									   consequent_condition,
									   precedence_type = c("directly_follows", "eventually_follows"),
									   reverse = FALSE) {

	pair <- NULL
	.trace <- NULL
	pattern <- NULL
	fits <- NULL
	n_fitting <- NULL


	precedence_type <- match.arg(precedence_type)

	conditions_valid <- NULL




	antecedent_condition_specified <- FALSE
	tryCatch({
		is.null(antecedent_condition)
	}, error = function(e) {
		antecedent_condition_specified <<- TRUE
	}
	)

	if(!antecedent_condition_specified) {
		# geen filter gespecifieerd.
		stop("No antecendent condition specified.")
	} else {
		antecedent_condition <- enquo(antecedent_condition)
		error_cond <- FALSE

		tryCatch({
			eventlog <- eventlog %>% mutate(ANTECEDENT_CONDITION = !!(antecedent_condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The antecedent condition (", expr_text(antecedent_condition), ") is not valid. Check the syntax and column names.")
		}
	}
	consequent_condition_specified <- FALSE
	tryCatch({
		is.null(consequent_condition)
	}, error = function(e) {
		consequent_condition_specified <<- TRUE
	}
	)

	if(!consequent_condition_specified) {
		# geen filter gespecifieerd.
		stop("No consequent condition specified")
	} else {
		consequent_condition <- enquo(consequent_condition)
		error_cond <- FALSE

		tryCatch({
			eventlog <- eventlog %>% mutate(CONSEQUENT_CONDITION = !!(consequent_condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The consequent condition (", expr_text(consequent_condition), ") is not valid. Check the syntax and column names.")
		}
	}

	eventlog %>%
		mutate(CONDITIONS = ifelse(ANTECEDENT_CONDITION, "antecedent_valid", ifelse(CONSEQUENT_CONDITION, "consequent_valid", "NA"))) %>%
		set_activity_id("CONDITIONS") -> eventlog_conditions

	pair <- "antecedent_valid,consequent_valid"


	if(precedence_type == "directly_follows") {
		pattern <- str_flatten(c(",", pair,","))
	} else if(precedence_type == "eventually_follows") {
		pattern <- str_flatten(c(",",map_chr(str_split(pair, ","), str_flatten, collapse = "(,NA)*,"),","))
	}

	eventlog_conditions %>%
		case_list() -> cases
	cases %>%
		distinct(trace) %>%
		mutate(.trace = glue(",{trace},")) %>%
		mutate(fits = str_detect(.trace, pattern)) %>%
		group_by(trace) %>%
		summarize(n_fitting = sum(fits)) -> check_results

	cases %>%
		left_join(check_results, by = "trace") -> cases_results

	case_selection <- filter(cases_results, n_fitting == 1) %>%
		pull(!!as.symbol(case_id(eventlog)))

	filter_case(eventlog, case_selection, reverse)

}

#' @describeIn filter_precedence Filter grouped event log
#' @export

filter_precedence_condition.grouped_eventlog <- function(eventlog,
														 antecedent_condition,
														 consequent_condition,
														 precedence_type = c("directly_follows", "eventually_follows"),
														 reverse = FALSE) {
	grouped_filter(eventlog, filter_precedence_condition, antecedent_condition, consequent_condition, precedence_type, reverse)
}

