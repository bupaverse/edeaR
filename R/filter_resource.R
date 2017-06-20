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
filter_resource <- function(eventlog,
							resources = NULL,
							reverse = F){
	stop_eventlog(eventlog)
	colnames(eventlog)[colnames(eventlog) == resource_id(eventlog)] <- "resource_classifier"


	if(reverse == F)
		output <- filter(eventlog, resource_classifier %in% resources)

	else
		output <- filter(eventlog, !(resource_classifier %in% resources))

	colnames(output)[colnames(output)=="resource_classifier"] <- resource_id(eventlog)

	output <- re_map(output, mapping(eventlog))

	return(output)
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
