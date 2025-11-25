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
#' @concept filters_event
#'
#' @export
filter_activity <- function(log, activities, reverse = FALSE) {
	UseMethod("filter_activity")
}

#' @describeIn filter_activity Filters activities for a \code{\link[bupaR]{log}}.
#' @export
filter_activity.log <- function(log, activities, reverse = FALSE) {

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
filter_activity.grouped_log <- function(log, activities, reverse = FALSE){


	apply_grouped_fun(log, fun = filter_activity.log, activities = activities, reverse = reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_activity Filter interactively
#' @export ifilter_activity
ifilter_activity <- function(log) {
	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter activities"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_activities", label = "Select activities:", choices = log %>% pull(!!as.symbol(activity_id(log))) %>%
								   	unique %>% sort, selected = NA,  multiple = TRUE), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list(activities = list(input$selected_activities), reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Activities", height = 400))

}
