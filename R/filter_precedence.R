#' @title Filter: precedence relations
#'
#' @description Filters cases based on the precedence relations between two sets of activities: antecedents and consequent.
#' The filter can detect directly following activities as well as eventually following activites.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param antecedents,consequents The set of antecendent and consequent activities. All pairs of antecedents and consequents are checked for.
#'
#' @param precedence_type When \code{directly_follows}, the consequent activity should happen immediately after the antecedent activities.
#' When \code{eventually_follows}, other events are allowed to happen in between.
#'
#' @param filter_method When \code{each}, only cases where all the relations are valid are preserved. When \code{one_of}, all the cases where
#' at least one of the conditions hold are preserved.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_precedence


filter_precedence <- function(eventlog,
							  antecedents,
							  consequents,
							  precedence_type = c("directly_follows", "eventually_follows"),
							  filter_method = c("each","one_of"),
							  reverse = F) {
	stop_eventlog(eventlog)

	precedence_type <- match.arg(precedence_type)
	filter_method <- match.arg(filter_method)


	if(precedence_type == "directly_follows")
		interleavings_allowed = FALSE
	else
		interleavings_allowed = TRUE

	sequences <- paste(rep(antecedents, each = length(consequents)),
					   rep(consequents, times = length(antecedents)), sep = ",")
	number_of_conditions <- length(sequences)

	patterns <- data.frame(pattern = sequences)
	tr <- traces(eventlog)

	dummies <- generate_pattern_dummies(patterns, eventlog, interleavings_allowed = interleavings_allowed)
	colnames(dummies)[colnames(dummies) == case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"

	dummies$conditions_valid <- rowSums(select(dummies, starts_with("X")))
	if(filter_method == "one_of")
		case_selection <- filter(dummies, conditions_valid > 0)$case_classifier
	else
		case_selection <- filter(dummies, conditions_valid == number_of_conditions)$case_classifier

	if(reverse == FALSE)
		f_eventlog <- filter(eventlog, case_classifier %in% case_selection)
	else
		f_eventlog <- filter(eventlog, !(case_classifier %in% case_selection))

	colnames(f_eventlog)[colnames(f_eventlog)=="case_classifier"] <- case_id(eventlog)

	output <- re_map(f_eventlog, mapping(eventlog))

	return(output)

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
							   	unique, selected = NA,  multiple = T), " ",
				selectizeInput("conse", label = "Select consequents:",
							   choices = eventlog %>% pull(!!as.symbol(activity_id(eventlog))) %>%
							   	unique, selected = NA,  multiple = T)),
				fillRow(
				radioButtons("type", "Precedence filter: ", choices = c("Directly follows" = "directly_follows", "Eventually follows"="eventually_follows"), selected = "directly_follows"),
				radioButtons("method", "Reverse filter: ", choices = c("Each" = "each", "One of" = "one_of"), selected = "each"),
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
										  reverse = ifelse(input$reverse == "Yes", T, F))


		stopApp(filtered_log)
	})
}
runGadget(ui, server, viewer = dialogViewer("Filter on precedences", height = 400))

}

