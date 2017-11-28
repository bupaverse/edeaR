#' Filter: Resource
#'
#' Filters the log based on resource identifiers
#'
#' #' The method filter_resource can be used to filter on resource identifiers. It has a resources argument,
#' to which a vector of identifiers can be given. The selection can be negated with the reverse argument.
#'
#' @param resources A vector of resources identifiers
#'
#' @inherit filter_activity params references seealso return
#'
#' @export filter_resource
#'
filter_resource <- function(eventlog, resources, reverse) {
	UseMethod("filter_resource")
}

#' @describeIn filter_resource Filter event log
#' @export


filter_resource.eventlog <- function(eventlog,
							resources,
							reverse = FALSE){

	if(reverse == F)
		filter(eventlog, (!!as.symbol(resource_id(eventlog))) %in% resources)
	else
		filter(eventlog, !((!!as.symbol(resource_id(eventlog))) %in% resources))
}

#' @describeIn filter_resource Filter grouped event log
#' @export

filter_resource.grouped_eventlog <- function(eventlog,
											 resources,
											 reverse = FALSE) {
	grouped_filter(eventlog, filter_resource, resources, reverse)
}

#' @rdname filter_resource
#' @export ifilter_resource
ifilter_resource <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter Resources"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_resources", label = "Select resources:", choices = eventlog %>% pull(!!as.symbol(resource_id(eventlog))) %>%
								   	unique %>% sort, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_resource(eventlog, resources = input$selected_resources, reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Resources", height = 400))

}
