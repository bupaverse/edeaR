#' @title Filter Activity
#'
#' @description Filters the log based on activities
#'
#' @param activities \code{\link{character}} vector: Containing one or more activity identifiers.
#' @param reverse \code{\link{logical}} (default \code{FALSE}): Indicating whether the selection should be reversed.
#'
#' @return When given an object of type \code{\link[bupaR]{log}}, it will return a filtered \code{\link[bupaR]{log}}.
#' When given an object of type \code{\link[bupaR]{grouped_log}}, the filter will be applied in a stratified way (i.e. each separately for each group).
#' The returned log will be grouped on the same variables as the original log.
#'
#' @inherit activity_frequency params references
#'
#' @seealso \code{vignette("filters", "edeaR")}
#'
#' @family filters
#'
#' @export
filter_activity <- function(log, activities, reverse = FALSE, eventlog = deprecated()) {
	UseMethod("filter_activity")
}

#' @describeIn filter_activity Filters activities for a \code{\link[bupaR]{log}}.
#' @export
filter_activity.log <- function(log, activities, reverse = FALSE, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	if (!reverse) {
		log %>%
			filter(.data[[activity_id(.)]] %in% activities)
	} else {
		log %>%
			filter(!(.data[[activity_id(.)]] %in% activities))
	}
}

#' @describeIn filter_activity Filters activities for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_activity.grouped_log <- function(log, activities, reverse = FALSE, eventlog = deprecated()){

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, fun = filter_activity.log, activities, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}


#' @export ifilter_activity
ifilter_activity <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_activity()")

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
