#' Filter: precedence relations with identical resources
#'
#' Filters cases based on the precedence relations between two sets of activities, where both antecendent and consequent have to be executed by the same resource.
#'
#' In order to extract a subset of an event log which conforms with a set of precedence rules, one can use the filter_precedence method. There are two types of
#' precendence relations which can be tested: activities that should directly follow each other,
#' or activities that should eventually follow each other. The type can be set with the precedence type argument.
#' Further, the filter requires a vector of one or more antecedents (containing activity labels), and one or more consequents. Finally, also a filter method argument
#' can be set. This argument is relevant when there is more than one antecedent or consequent.
#' In such a case, you can specify that all possible precedence combinations must be present (all), at least one of them (one of), or none (none).
#' In case an activity instance exists of more than one events with different resource identifiers, only the first will be considered.
#'
#' @param antecedents,consequents The set of antecendent and consequent activities. Both are character vectors containing at least one activity identifier.
#' All pairs of antecedents and consequents are turned into seperate precedence rules.
#'
#' @param precedence_type When \code{directly_follows}, the consequent activity should happen immediately after the antecedent activities.
#' When \code{eventually_follows}, other events are allowed to happen in between.
#'
#' @param filter_method When \code{all}, only cases where all the relations are valid are preserved. When \code{one_of}, all the cases where
#' at least one of the conditions hold are preserved. When \code{none}, none of the relations are allowed.
#'
#' @inherit filter_activity params references seealso return
#' @importFrom forcats fct_recode
#' @export filter_precedence_resource

filter_precedence_resource <- function(eventlog,
							  antecedents,
							  consequents,
							  precedence_type,
							  filter_method,
							  reverse) {
	UseMethod("filter_precedence_resource")
}

#' @export

filter_precedence_resource.eventlog <- function(eventlog,
									   antecedents,
									   consequents,
									   precedence_type = c("directly_follows", "eventually_follows"),
									   filter_method = c("all","one_of", "none"),
									   reverse = FALSE) {
	precedence_type <- match.arg(precedence_type)
	filter_method <- match.arg(filter_method)

	# TEST VALIDITY ACTIVITY LABELS
	acts <- activity_labels(eventlog)
	wrong <-  antecedents[!(antecedents %in% acts)]
	if(length(wrong) > 0) {
		warning(glue("{length(wrong)} specified antecedents not found in event log, and removed: {str_flatten(wrong, collapse = ', ')}"))
		antecedents <- antecedents[(antecedents %in% acts)]
	}
	wrong <-  consequents[!(consequents %in% acts)]
	if(length(wrong) > 0) {
		warning(glue("{length(wrong)} specified consequents not found in event log, and removed: {str_flatten(wrong, collapse = ', ')}"))
		consequents <- consequents[(consequents %in% acts)]
	}

	if(length(antecedents) < 1 || length(consequents) < 1) {
		stop("No valid antecendent-consequent pairs.")
	}

	# CREATE FLOWS TO BE CHECKED
	sequence_pairs <- crossing(antecedent = antecedents, consequent = consequents)
	number_of_conditions <- nrow(sequence_pairs)

	output <- list_along(1:number_of_conditions)

	## CHECK EACH FLOW
	for(i in 1:number_of_conditions) {
		output[[i]] <- filter_precedence_resource_single(eventlog, sequence_pairs$antecedent[i], sequence_pairs$consequent[i])
	}


	# COMPUTE NUMBER OF FLOWS FOUND
	if(precedence_type == "directly_follows") {
		output %>%
			bind_rows() %>%
			filter(directly) %>%
			count(case_id, name = "n_fitting") -> cases_results

	} else if(precedence_type == "eventually_follows") {
		output %>%
			bind_rows() %>%
			filter(eventually) %>%
			count(case_id, name = "n_fitting") -> cases_results
	}

	# CREATE CASE SELECTION
	if(filter_method == "one_of")
		case_selection <- filter(cases_results, n_fitting > 0) %>% pull(!!as.symbol(case_id(eventlog)))
	else if(filter_method == "all")
		case_selection <- filter(cases_results, n_fitting == number_of_conditions) %>% pull(!!as.symbol(case_id(eventlog)))
	else if(filter_method == "none")
		case_selection <- filter(cases_results, n_fitting == 0) %>% pull(!!as.symbol(case_id(eventlog)))

	#FILTER CASES
	filter_case(eventlog, case_selection, reverse)

}

#' @export

filter_precedence_resource.grouped_eventlog <- function(eventlog,
											   antecedents,
											   consequents,
											   precedence_type = c("directly_follows", "eventually_follows"),
											   filter_method = c("all","one_of", "none"),
											   reverse = FALSE) {
	grouped_filter(eventlog, filter_precedence_resource, antecedents, consequents, precedence_type, filter_method, reverse)
}


filter_precedence_resource_single <- function(eventlog, antecedent, consequent) {
	eventlog %>%
		create_minimal_activity_log() %>%
		## ADD RANK TO EACH ACTIVITY
		group_by(!!case_id_(eventlog)) %>%
		arrange(time, min_order) %>%
		mutate(rank = 1:n()) %>%
		## RECODE FLOW ACTIVITIES
		mutate(activity_instance_classifier = fct_recode(!!activity_id_(eventlog),
														 antecedent = antecedent,
														 consequent = consequent)) %>%
		## FILTER FLOW ACTIVITIES
		filter(activity_instance_classifier %in% c("antecedent","consequent")) %>%
		select(resource_identifier,!!case_id_(eventlog), activity_instance_classifier, rank) %>%
		# NEST RANKS OF ANTECENDENT ANF CONSEQUENT
		group_by(resource_identifier,!!case_id_(eventlog), activity_instance_classifier) %>%
		nest() %>%
		ungroup() %>%
		# RENAME NESTED RANK VARIABLE
		mutate(data = map2(data, activity_instance_classifier, ~set_names(.x, .y))) %>%
		# PUT RANKS NEXT O EACH OTHER
		spread(activity_instance_classifier, data) %>%
		# DELETE ROW WHERE ANTECEDENT OF CONSEQUENT IS NOT FOUND
		filter(!map_lgl(antecedent, is.null), !map_lgl(consequent, is.null)) %>%
		# COMPARE ALL POSSIBLE RANK COMBINATIONS
		mutate(t = map2(antecedent, consequent, merge)) %>%
		# ARE ANY DIRECTLY OR EVENTUALLY FOLLOWING?
		mutate(directly = map_lgl(t, ~(nrow(filter(.x, antecedent + 1 == consequent)) > 0))) %>%
		mutate(eventually = map_lgl(t, ~(nrow(filter(.x, antecedent < consequent)) > 0))) %>%
		# DISTINCT TO COMBINE OBSERVATIONS FOR MORE THAN ONE RESOURCe
		distinct(case_id, directly, eventually)

}



