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
#' @param antecedents,consequents The set of antecendent and consequent activities. Both are character vectors containing at leaste one activity identifier.
#' All pairs of antecedents and consequents are turned into seperate precedence rules.
#'
#' @param precedence_type When \code{directly_follows}, the consequent activity should happen immediately after the antecedent activities.
#' When \code{eventually_follows}, other events are allowed to happen in between.
#'
#' @param filter_method When \code{each}, only cases where all the relations are valid are preserved. When \code{one_of}, all the cases where
#' at least one of the conditions hold are preserved.
#'
#' @inherit filter_activity params references seealso return
#'
#' @export filter_precedence

filter_precedence <- function(eventlog,
							  antecedents,
							  consequents,
							  precedence_type,
							  filter_method,
							  reverse) {
	UseMethod("filter_precedence")
}

#' @describeIn filter_precedence Filter event log
#' @export

filter_precedence.eventlog <- function(eventlog,
									   antecedents,
									   consequents,
									   precedence_type = c("directly_follows", "eventually_follows"),
									   filter_method = c("all","one_of", "none"),
									   reverse = FALSE) {

	conditions_valid <- NULL

	precedence_type <- match.arg(precedence_type)
	filter_method <- match.arg(filter_method)

	interleavings_allowed <- ifelse(precedence_type == "directly_follows", FALSE, TRUE)

	sequences <- paste(rep(antecedents, each = length(consequents)),
					   rep(consequents, times = length(antecedents)), sep = ",")

	number_of_conditions <- length(sequences)

	patterns <- data.frame(pattern = sequences)

	dummies <- generate_pattern_dummies(patterns, eventlog, interleavings_allowed = interleavings_allowed)

	dummies$conditions_valid <- rowSums(select(dummies, starts_with("X")))

	if(filter_method == "one_of")
		case_selection <- filter(dummies, conditions_valid > 0) %>% pull(!!as.symbol(case_id(eventlog)))
	else if(filter_method == "all")
		case_selection <- filter(dummies, conditions_valid == number_of_conditions) %>% pull(!!as.symbol(case_id(eventlog)))
	else if(filter_method == "none")
		case_selection <- filter(dummies, conditions_valid == 0) %>% pull(!!as.symbol(case_id(eventlog)))

	filter_case(eventlog, case_selection, reverse)

}

#' @describeIn filter_precedence Filter grouped event log
#' @export

filter_precedence.grouped_eventlog <- function(eventlog,
											   antecedents,
											   consequents,
											   precedence_type = c("directly_follows", "eventually_follows"),
											   filter_method = c("all","one_of", "none"),
											   reverse = FALSE) {
	grouped_filter(eventlog, filter_precedence, antecedents, consequents, precedence_type, filter_method, reverse)
}


#' @rdname filter_precedence
#' @export ifilter_precedence
#'
ifilter_precedence <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter on precedences"),
		miniContentPanel(
			fillCol(flex = c(5,3,2),
					fillRow(flex = c(10,1,10),
							selectizeInput("ante", label = "Select antecedents:",
										   choices = eventlog %>% pull(!!as.symbol(activity_id(eventlog))) %>%
										   	unique, selected = NA,  multiple = TRUE), " ",
							selectizeInput("conse", label = "Select consequents:",
										   choices = eventlog %>% pull(!!as.symbol(activity_id(eventlog))) %>%
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

			filtered_log <- filter_precedence(eventlog,
											  antecedents = input$ante,
											  consequents = input$conse,
											  precedence_type = input$type,
											  filter_method = input$method,
											  reverse = ifelse(input$reverse == "Yes", TRUE, FALSE))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter on precedences", height = 400))

}

