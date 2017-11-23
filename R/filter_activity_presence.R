#' @title Filter: Activity Presence
#'
#' @description Filters cases based on the presence (or absence) of activities
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param activities A vector of activities to withhold.
#'
#' @param method Filter method. If "all", each of the activities should be present. If "one_of", at least one of them should be present. If "none", none of the activities are allowed to occur in the filtered traces.
#'
#' @export filter_activity_presence
filter_activity_presence <- function(eventlog, activities, method, reverse) {
	UseMethod("filter_activity_presence")
}


#' @describeIn filter_activity_presence Filter event log on presence of activities.
#' @export

filter_activity_presence.eventlog <- function(eventlog,
											  activities = NULL,
											  method = c("all", "one_of", "none"),
											  reverse = FALSE){
	method <- match.arg(method)

	eventlog %>%
		filter_activity(activities) %>%
		select(!!as.symbol(activity_id(eventlog)), !!as.symbol(case_id(eventlog))) %>%
		unique() %>%
		group_by(!!as.symbol(case_id(eventlog))) %>%
		summarize(n = n()) -> selection

		if(method == "all")
		filter_case(eventlog, selection %>% filter(n == length(activities)) %>% pull(1), reverse)
	else if(method == "one_of")
		filter_case(eventlog, selection %>% pull(1), reverse)
	else if (method == "none")
		filter_case(eventlog, selection %>% pull(1), reverse = !reverse)
}

#' @describeIn filter_activity_presence Filter grouped event log on presence of activities.
#' @export
filter_activity_presence.grouped_eventlog <- function(eventlog,
											  activities = NULL,
											  method = c("all", "one_of", "none"),
											  reverse = FALSE) {
	grouped_filter(eventlog, filter_activity_presence, activities, method)
}

#' @rdname filter_activity_presence
#' @export ifilter_activity_presence

ifilter_activity_presence <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter activities based on presence"),
		miniContentPanel(
			fillCol(flex = c(2,1),
					fillRow(flex = c(10,1,8),
							selectizeInput("selected_activities",
										   label = "Select activities:",
										   choices = eventlog %>% pull(!!as.symbol(activity_id(eventlog))) %>%
										   	unique, selected = NA,  multiple = TRUE), " ",
							radioButtons("method", "Method: ", choices = c("All" = "all","One of"= "one_of","None" = "none"), selected = "all")
					),
					"If \"all\", each of the activities should be present.
					If \"one_of\", at least one of them should be present. If \"none\", none of the activities are allowed to occur in the filtered traces."
			))
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_activity_presence(eventlog,
													 activities = input$selected_activities,
													 method = input$method)


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter activities based on presence", height = 400))

}
