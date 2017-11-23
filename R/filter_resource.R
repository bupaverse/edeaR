#' @title Filter: Resource
#'
#' @description Filters the log based on resources
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param resources A vector of resources to withhold
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
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
