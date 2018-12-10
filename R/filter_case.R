#' title Filter: Case
#'
#' Filters the log based on case identifier
#'
#' The method filter_case can be used to filter on case identifiers. It has an cases argument,
#' to which a vector of identifiers can be given. The selection can be negated with the reverse argument.
#'
#' @param cases A vector of cases identifiers
#'
#' @inherit filter_activity params references seealso return
#' @export filter_case

filter_case <- function(eventlog, cases, reverse) {
	UseMethod("filter_case")
}


#' @describeIn filter_case Filter for eventlogs
#' @export
filter_case.eventlog <- function(eventlog,
							cases = NULL,
							reverse = FALSE){

	if(!reverse)
		filter(eventlog, (!!as.symbol(case_id(eventlog))) %in% cases)
	else
		filter(eventlog, !((!!as.symbol(case_id(eventlog))) %in% cases))
}

#' @describeIn filter_case Stratified filter for grouped eventlogs
#' @export

filter_case.grouped_eventlog <- function(eventlog, cases = NULL, reverse = FALSE) {
	grouped_filter(eventlog, filter_case, cases, reverse)
}

#' @rdname filter_case
#' @export ifilter_case

ifilter_case <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter Cases"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_cases", label = "Select cases:", choices = eventlog %>% pull(!!as.symbol(case_id(eventlog))) %>%
								   	unique, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_case(eventlog, cases = input$selected_cases, reverse = ifelse(input$reverse == "Yes", T, F))

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Cases", height = 400))

}
