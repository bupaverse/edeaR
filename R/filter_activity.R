#' @title Filter: Activity
#'
#' @description Filters the log based on activities
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param activities A vector of activities to withhold
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_activity
#'
filter_activity <- function(eventlog, activities, reverse) {
	UseMethod("filter_activity")
}

#' @describeIn filter_activity Filter eventlog for activity labels
#' @export

filter_activity.eventlog <- function(eventlog,
									 activities,
									 reverse = FALSE){
	if(reverse == FALSE)
		filter(eventlog, (!!as.symbol(activity_id(eventlog))) %in% activities)
	else
		filter(eventlog, !((!!as.symbol(activity_id(eventlog))) %in% activities))
}

#' @describeIn filter_activity Filter grouped eventlog for activity labels
#' @export

filter_activity.grouped_eventlog <- function(eventlog,
											 activities,
											 reverse = FALSE){

	grouped_filter(eventlog, filter_activity, activities, reverse)
}


#' @rdname filter_activity
#' @export ifilter_activity
ifilter_activity <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter activities"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_activities", label = "Select activities:", choices = eventlog %>% pull(!!as.symbol(activity_id(eventlog))) %>%
								   	unique %>% sort, selected = NA,  multiple = TRUE), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_activity(eventlog,
											activities = input$selected_activities,
											reverse = ifelse(input$reverse == "Yes", TRUE, FALSE))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Activities", height = 400))

}
