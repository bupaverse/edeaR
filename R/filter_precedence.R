#' @title Filter Precedence Relations
#'
#' @description Filters cases based on the precedence relations between two sets of activities.
#'
#' @param antecedents,consequents \code{\link{character}} vector: The set of antecendent and consequent activities.
#' Both are \code{\link{character}} vectors containing at least one activity identifier. All pairs of antecedents and consequents are turned into seperate precedence rules.
#' @param precedence_type \code{\link{character}} (default \code{"directly_follows"}): When \code{"directly_follows"},
#' the consequent activity should happen immediately after the antecedent activities.\cr
#' When \code{"eventually_follows"}, other events are allowed to happen in between.
#' @param filter_method \code{\link{character}} (default \code{"all"}): When \code{"all"}, only cases where all the relations are valid are preserved.\cr
#' When \code{"one_of"}, all the cases where at least one of the conditions hold, are preserved.\cr
#' When \code{"none"}, none of the relations are allowed.
#'
#' @details
#' In order to extract a subset of an event log which conforms with a set of precedence rules, one can use the \code{filter_precedence} method.
#' There are two types of precendence relations which can be tested: activities that should directly follow (\code{"directly_follows"}) each other,
#' or activities that should eventually follow (\code{"eventually_follows"}) each other. The type can be set with the \code{precedence_type} argument.
#'
#' Further, the filter requires a vector of one or more \code{antecedents} (containing activity labels), and one or more \code{consequents}.
#'
#' Finally, a \code{filter_method} argument can be set. This argument is relevant when there is more than one antecedent or consequent.
#' In such a case, you can specify that all possible precedence combinations must be present (\code{"all"}),
#' at least one of them (\code{"one_of"}), or none (\code{"none"}).
#'
#' @examples
#'
#' eventdataR::patients %>%
#' 	filter_precedence(antecedents = "Triage and Assessment",
#' 					  consequents = "Blood test",
#' 					  precedence_type = "directly_follows") %>%
#' 	bupaR::traces()
#'
#' eventdataR::patients %>%
#' 	filter_precedence(antecedents = "Triage and Assessment",
#' 					  consequents = c("Blood test", "X-Ray", "MRI SCAN"),
#' 					  precedence_type = "eventually_follows",
#' 					  filter_method = "one_of") %>%
#' 	bupaR::traces()
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_precedence
filter_precedence <- function(log,
							  antecedents,
							  consequents,
							  precedence_type = c("directly_follows", "eventually_follows"),
							  filter_method = c("all", "one_of", "none"),
							  reverse = FALSE) {
	UseMethod("filter_precedence")
}

#' @describeIn filter_precedence Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_precedence.log <- function(log,
								  antecedents,
								  consequents,
								  precedence_type = c("directly_follows", "eventually_follows"),
								  filter_method = c("all", "one_of", "none"),
								  reverse = FALSE) {

	pair <- NULL
	.trace <- NULL
	pattern <- NULL
	fits <- NULL
	n_fitting <- NULL

	precedence_type <- rlang::arg_match(precedence_type)
	filter_method <- rlang::arg_match(filter_method)

	conditions_valid <- NULL
	acts <- activity_labels(log)

	wrong <- antecedents[!(antecedents %in% acts)]
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

	sequence_pairs <- tibble(pair = paste(rep(antecedents, each = length(consequents)),
						   			rep(consequents, times = length(antecedents)), sep = ","))

	number_of_conditions <- nrow(sequence_pairs)


	sequence_pairs %>%
		rowwise %>%
		mutate(pattern = str_flatten(c(",", pair,","))) -> sequence_pairs


	log %>%
		case_list() -> cases

	if(precedence_type == "directly_follows") {
		log %>%
			case_list() -> cases

	} else if(precedence_type == "eventually_follows") {
		log %>%
			filter_activity(activities = c(antecedents, consequents)) %>%
			case_list() -> cases
	}

	cases %>%
		distinct(trace) %>%
		mutate(.trace = glue(",{trace},")) %>%
		inner_join(sequence_pairs, by = character()) %>%
		mutate(fits = str_detect(.trace, fixed(pattern))) %>%
		group_by(trace) %>%
		summarize(n_fitting = sum(fits)) -> check_results

	cases %>%
		left_join(check_results, by = "trace") ->
		cases_results

	if(filter_method == "one_of")
		case_selection <- filter(cases_results, n_fitting > 0) %>% pull(!!as.symbol(case_id(log)))
	else if(filter_method == "all")
		case_selection <- filter(cases_results, n_fitting == number_of_conditions) %>% pull(!!as.symbol(case_id(log)))
	else if(filter_method == "none")
		case_selection <- filter(cases_results, n_fitting == 0) %>% pull(!!as.symbol(case_id(log)))

	filter_case.log(log, cases = case_selection, reverse = reverse)
}

#' @describeIn filter_precedence Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_precedence.grouped_log <- function(log,
										  antecedents,
										  consequents,
										  precedence_type = c("directly_follows", "eventually_follows"),
										  filter_method = c("all", "one_of", "none"),
										  reverse = FALSE) {

	apply_grouped_fun(log, fun = filter_precedence.log, antecedents, consequents, precedence_type, filter_method, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_precedence, antecedents, consequents, precedence_type, filter_method, reverse)
}

#' @describeIn filter_precedence Filter interactively
#' @export ifilter_precedence
ifilter_precedence <- function(log) {

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter on precedences"),
		miniContentPanel(
			fillCol(flex = c(5,3,2),
					fillRow(flex = c(10,1,10),
							selectizeInput("ante", label = "Select antecedents:",
										   choices = log %>% pull(!!as.symbol(activity_id(log))) %>%
										   	unique, selected = NA,  multiple = TRUE), " ",
							selectizeInput("conse", label = "Select consequents:",
										   choices = log %>% pull(!!as.symbol(activity_id(log))) %>%
										   	unique, selected = NA,  multiple = TRUE)),
					fillRow(
						radioButtons("type", "Precedence filter: ", choices = c("Directly follows" = "directly_follows", "Eventually follows"="eventually_follows"), selected = "directly_follows"),
						radioButtons("method", "Reverse filter: ", choices = c("All" = "all", "One of" = "one_of", "None" = "none"), selected = "all"),
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")),
					"When directly_follows, the consequent activity should happen immediately after the antecedent activities. When eventually_follows, other events are allowed to happen in between. When each, only cases where all the relations are valid are preserved. When one_of, all the cases where at least one of the conditions hold are preserved."
			)

		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list(antecedents = list(input$ante),
													   consequents = list(input$conse),
													   precedence_type = list(input$type, "'directly_follows'"),
													   filter_method = list(input$method, "'all'"),
													   reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter on precedences", height = 400))

}

