#' @title Filter Start and End Activities
#'
#' @description Filters the log based on a provided set of start and end activities
#'
#' The \code{filter_endpoints} method filters cases based on the first and last activity label. It can be used in two ways: by specifying vectors with allowed start
#' activities and/or allowed end activities, or by specifying a percentile. In the latter case, the percentile value will be used as a cut off.
#' For example, when set to \code{0.9}, it will select the most common endpoint pairs which together cover at least 90% of the cases, and filter the log accordingly.
#'
#' @param start_activities,end_activities \code{\link{character}} vector (default \code{\link{NULL}}): A vector of activity identifiers, or \code{\link{NULL}}.
#' @param percentage \code{\link{numeric}} (default \code{\link{NULL}}): A percentage p to be used as percentile cut off. When this is used, the most common endpoint-pairs will be selected
#' until at least the p% of the cases are selected.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_endpoints
filter_endpoints <- function(log,
							 start_activities = NULL,
							 end_activities = NULL,
							 percentage = NULL,
							 reverse = FALSE) {
	UseMethod("filter_endpoints")
}

#' @describeIn filter_endpoints Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_endpoints.log <- function(log,
								 start_activities = NULL,
								 end_activities = NULL,
								 percentage = NULL,
								 reverse = FALSE) {

	if(is.null(start_activities) & is.null(end_activities) & is.null(percentage))
		stop("At least one set of start or end activities or a percentage must be provided.")
	if((!is.null(start_activities) & !is.null(percentage)) | (!is.null(end_activities) & !is.null(percentage)))
		stop("Cannot filter on both sets of start and end activities and percentage simultaneously.")
	if(!is.null(percentage))
		filter_endpoints_percentile(log, percentage = percentage, reverse = reverse)
	else
		filter_endpoints_sets(log, start_activities = start_activities, end_activities = end_activities, reverse = reverse)
}

#' @describeIn filter_endpoints Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_endpoints.grouped_log <- function(log,
										 start_activities = NULL,
										 end_activities = NULL,
										 percentage = NULL,
										 reverse = FALSE) {

	apply_grouped_fun(log, fun = filter_endpoints.log, start_activities, end_activities, percentage, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_endpoints, start_activities, end_activities, percentage, reverse, ...)
}

#' @describeIn filter_endpoints Filter interactively
#' @export ifilter_endpoints
ifilter_endpoints <- function(log) {
	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter End Points"),
		miniContentPanel(
			fillCol(flex = c(3,8),
					fillRow(
						radioButtons("filter_type", "Filter type:", choices = c("Select from list" = "list", "Use percentile cutoff" = "percentile")),
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
					),
					uiOutput("filter_ui")
					)
		)
	)

	server <- function(input, output, session){

		output$filter_ui <- renderUI({
			if(input$filter_type == "list") {
				fillRow(flex = c(8,1,8),
					selectizeInput("start", label = "Select start activities:",
								   choices = log %>%
								   	start_activities_activity %>%
								   	pull(!!as.symbol(activity_id(log))) %>%
								   	unique, selected = NA,  multiple = TRUE), " ",
					selectizeInput("end", label = "Select end activities:",
								   choices = log %>%
								   	end_activities_activity %>%
								   	pull(!!as.symbol(activity_id(log))) %>%
								   	unique, selected = NA,  multiple = TRUE)
				) %>% return()
			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentile cut off:", min = 0, max = 100, value = 80) %>% return()
			}
		})



		observeEvent(input$done, {
			if(input$filter_type == "list")
				fun_call <- construct_call(input_cmd, list(start_activities = list(input$start), end_activities = list(input$end), reverse = list(input$reverse == "Yes", FALSE)))

			else if(input$filter_type == "percentile") {
				fun_call <- construct_call(input_cmd, list(percentage = list(input$percentile_slider/100), reverse = list(input$reverse == "Yes", FALSE)))

			}
			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter End Points", height = 400))

}


