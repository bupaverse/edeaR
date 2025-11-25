#' @title Filter Precedence Relations
#'
#' @description Filters cases based on the precedence relations between two sets of activities. For more information, see \code{\link{filter_precedence}}.
#'
#' @param antecedent_condition,consequent_condition The antecendent and consequent conditions.
#'
#' @inherit filter_precedence params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_precedence_condition
filter_precedence_condition <- function(log,
										antecedent_condition,
										consequent_condition,
										precedence_type = c("directly_follows", "eventually_follows"),
										reverse = FALSE) {
	UseMethod("filter_precedence_condition")
}

#' @describeIn filter_precedence_condition Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_precedence_condition.log <- function(log,
											antecedent_condition,
											consequent_condition,
											precedence_type = c("directly_follows", "eventually_follows"),
											reverse = FALSE) {

	ANTECEDENT_CONDITION <- NULL
	CONSEQUENT_CONDITION <- NULL

	pair <- NULL
	.trace <- NULL
	pattern <- NULL
	fits <- NULL
	n_fitting <- NULL

	precedence_type <- rlang::arg_match(precedence_type)

	conditions_valid <- NULL

	antecedent_condition_specified <- FALSE
	tryCatch({
		is.null(antecedent_condition)
	}, error = function(e) {
		antecedent_condition_specified <<- TRUE
	})

	if(!antecedent_condition_specified) {
		# geen filter gespecifieerd.
		stop("No antecendent condition specified.")
	} else {
		antecedent_condition <- enquo(antecedent_condition)
		error_cond <- FALSE

		tryCatch({
			log <- log %>% mutate(ANTECEDENT_CONDITION = !!(antecedent_condition))
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
	})

	if(!consequent_condition_specified) {
		# geen filter gespecifieerd.
		stop("No consequent condition specified")
	} else {
		consequent_condition <- enquo(consequent_condition)
		error_cond <- FALSE

		tryCatch({
			log <- log %>% mutate(CONSEQUENT_CONDITION = !!(consequent_condition))
		}, error = function(e) {
			error_cond <<- TRUE
		})

		if(error_cond) {
			stop("The consequent condition (", expr_text(consequent_condition), ") is not valid. Check the syntax and column names.")
		}
	}

	log %>%
		mutate(CONDITIONS = ifelse(ANTECEDENT_CONDITION, "antecedent_valid", ifelse(CONSEQUENT_CONDITION, "consequent_valid", "NA"))) %>%
		set_activity_id("CONDITIONS") -> log_conditions

	pair <- "antecedent_valid,consequent_valid"

	if(precedence_type == "directly_follows") {
		pattern <- str_flatten(c(",", pair,","))
	} else if(precedence_type == "eventually_follows") {
		pattern <- str_flatten(c(",",map_chr(str_split(pair, ","), str_flatten, collapse = "(,NA)*,"),","))
	}

	log_conditions %>%
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
		pull(!!as.symbol(case_id(log)))

	filter_case.log(log, cases = case_selection, reverse = reverse)
}

#' @describeIn filter_precedence_condition Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_precedence_condition.grouped_log <- function(log,
													antecedent_condition,
													consequent_condition,
													precedence_type = c("directly_follows", "eventually_follows"),
													reverse = FALSE) {

	apply_grouped_fun(log, fun = filter_precedence.log, antecedent_condition, consequent_condition, precedence_type, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_precedence_condition, antecedent_condition, consequent_condition, precedence_type, reverse)
}

