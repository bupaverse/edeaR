#' @title Filter Activity Instance
#'
#' @description Filters the log based on activity instance identifier. This method has an \code{activity_instances} argument,
#' to which a vector of identifiers can be given. The selection can be negated with the \code{reverse} argument.
#'
#' @param log \code{\link[bupaR]{eventlog}}: Object of class \code{\link[bupaR]{eventlog}} or derivatives (\code{\link[bupaR]{grouped_eventlog}}).
#' @param activity_instances A vector of activity instance identifiers.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_event
#'
#' @export filter_activity_instance
filter_activity_instance <- function(log,
									 activity_instances,
									 reverse = FALSE) {
	UseMethod("filter_activity_instance")
}

#' @describeIn filter_activity_instance Filters activities for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_activity_instance.eventlog <- function(log,
											  activity_instances,
											  reverse = FALSE) {

	if(!reverse) {
		log %>%
			filter(.data[[activity_instance_id(.)]] %in% activity_instances)
	} else {
		log %>%
			filter(!(.data[[activity_instance_id(.)]] %in% activity_instances))
	}
}

#' @describeIn filter_activity_instance Filters activities for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
filter_activity_instance.grouped_eventlog <- function(log,
													  activity_instances,
													  reverse = FALSE) {

	bupaR:::apply_grouped_fun(log, fun = filter_activity_instance.eventlog, activity_instances, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_activity_instance Filter interactively
#' @export ifilter_activity_instance
ifilter_activity_instance <- function(log) {

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))


	ui <- miniPage(
		gadgetTitleBar("Filter Activity Instances"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_aid", label = "Select activity instances:", choices = NULL, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		updateSelectizeInput(session, "selected_aid", choices = log %>% pull(!!as.symbol(activity_instance_id(log))) %>% unique(), server = T)

		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list("activity_instances" = list(input$selected_aid), "reverse" = list(input$reverse == "Yes", FALSE)))

			rstudioapi::sendToConsole(fun_call)
			stopApp()
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Activity instances", height = 400))

}
