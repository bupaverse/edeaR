#' title Filter: Activity instance
#'
#' Filters the log based on activity instance identifier
#'
#' The method filter_activity_instance can be used to filter on activity instance identifiers. It has an activity_instances argument,
#' to which a vector of identifiers can be given. The selection can be negated with the reverse argument.
#'
#' @param activity_instances A vector of activity instance identifiers
#'
#' @inherit filter_activity params references seealso return
#' @export filter_activity_instance

filter_activity_instance <- function(eventlog,
									 activity_instances,
									 reverse) {
	UseMethod("filter_activity_instance")
}


#' @describeIn filter_activity_instance Filter for eventlogs
#' @export
filter_activity_instance.eventlog <- function(eventlog,
											  activity_instances = NULL,
											  reverse = FALSE){

	if(!reverse)
		filter(eventlog, (!!activity_instance_id_(eventlog)) %in% activity_instances)
	else
		filter(eventlog, !((!!activity_instance_id_(eventlog)) %in% activity_instances))
}

#' @describeIn filter_activity_instance Stratified filter for grouped eventlogs
#' @export

filter_activity_instance.grouped_eventlog <- function(eventlog,
													  activity_instances = NULL,
													  reverse = FALSE) {
	grouped_filter(eventlog, filter_activity_instance, cases, reverse)
}

#' @rdname filter_activity_instance
#' @export ifilter_activity_instance

ifilter_activity_instance <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter Activity Instances"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_cases", label = "Select activity instances:", choices = eventlog %>% pull(!!as.symbol(activity_instance_id(eventlog))) %>%
								   	unique, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_activity_instance(eventlog,
													 activity_instances = input$selected_cases,
													 reverse = ifelse(input$reverse == "Yes", T, F))

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Activity instances", height = 400))

}
