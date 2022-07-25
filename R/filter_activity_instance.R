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
#' @export filter_activity_instance
filter_activity_instance <- function(log,
									 activity_instances,
									 reverse = FALSE,
									 eventlog = deprecated()) {
	UseMethod("filter_activity_instance")
}

#' @describeIn filter_activity_instance Filters activities for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_activity_instance.eventlog <- function(log,
											  activity_instances,
											  reverse = FALSE,
											  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

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
													  reverse = FALSE,
													  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, fun = filter_activity_instance.eventlog, activity_instances, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @keywords internals
#' @rdname filter_activity_instance
#' @export ifilter_activity_instance
ifilter_activity_instance <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_activity_instance()")

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
