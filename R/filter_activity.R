#' Filter: Activity
#'
#' Filters the log based on activities
#'
#' The method filter_activity can be used to filter on activity identifiers. It has an activities argument,
#' to which a vector of identifiers can be given. The selection can be negated with the reverse argument.
#'
#' @param eventlog The dataset to be used. Should be a (grouped) eventlog object.
#'
#' @param activities Character vector containing one or more activity identifiers.
#' @param reverse Logical, indicating whether the selection should be reversed.
#' @param ... Deprecated arguments.
#'
#' @seealso \code{vignette("filters", "edeaR")}
#'
#' @return When given an eventlog, it will return a filtered eventlog. When given a grouped eventlog, the filter will be applied
#' in a stratified way (i.e. each separately for each group). The returned eventlog will be grouped on the same variables as
#' the original event log.
#'
#' @export
filter_activity <- function(eventlog, activities, reverse, ...) {
	UseMethod("filter_activity")
}

#' @describeIn filter_activity Filter eventlog for activity labels
#' @export

filter_activity.eventlog <- function(eventlog,
									 activities,
									 reverse = FALSE,
									 ...){
	if(reverse == FALSE)
		filter(eventlog, (!!as.symbol(activity_id(eventlog))) %in% activities)
	else
		filter(eventlog, !((!!as.symbol(activity_id(eventlog))) %in% activities))
}

#' @describeIn filter_activity Filter grouped eventlog for activity labels
#' @export

filter_activity.grouped_eventlog <- function(eventlog,
											 activities,
											 reverse = FALSE,
											 ...){

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
