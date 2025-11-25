#' @title Filter Resource Frequency
#'
#' @description Filters the log based on frequency of resources
#'
#' @param percentage,interval The target coverage of activity instances. Provide either \code{percentage} or \code{interval}.\cr
#' \code{percentage} (\code{\link{numeric}}): A percentile of p will return the most common resource types of the log,
#' which account for at least p% of the activity instances.\cr
#' \code{interval} (\code{\link{numeric}} vector of length 2): A resource frequency interval. Half open interval can be created using \code{\link{NA}}.\cr
#' For more information, see 'Details' below.
#'
#' @details
#' Filtering the log based on resource frequency can be done in two ways: using an \code{interval} of allowed frequencies,
#' or specify a coverage \code{percentage}:
#'
#' \itemize{
#' \item \code{percentage}: When filtering using a percentage p%, the filter will return p% of the activity instances,
#' starting from the resource labels with the highest frequency. The filter will retain additional resource labels
#' as long as the number of activity instances does not exceed the percentage threshold.
#' \item \code{interval}: When filtering using an interval, resource labels will be retained when their absolute frequency fall in this interval.
#' The interval is specified using a numeric vector of length 2. Half open intervals can be created by using \code{\link{NA}},
#' e.g., \code{c(10, NA)} will select resource labels which occur 10 times or more.
#' }
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_event
#'
#' @export filter_resource_frequency
filter_resource_frequency <- function(log,
									  interval = NULL,
									  percentage = NULL,
									  reverse = FALSE) {
	UseMethod("filter_resource_frequency")
}

#' @describeIn filter_resource_frequency Filters resources for a \code{\link[bupaR]{log}}.
#' @export
filter_resource_frequency.log <- function(log,
										  interval = NULL,
										  percentage = NULL,
										  reverse = FALSE) {

	stopifnot(is.logical(reverse))
	mapping <- mapping(log)

	if(!is.null(interval) && !is.null(percentage)) {
		stop("Provide interval OR percentage Cannot filter using both methods.")
	} else if(!is.null(interval)) {
		stopifnot(is.numeric(interval))
		if(length(interval) != 2 || any(interval < 0, na.rm = T) || all(is.na(interval))){
			stop("Interval should be a positive numeric vector of length 2. One of the elements can be NA to create open intervals.")
		} else {
			filter_resource_interval(log, interval[1], interval[2], reverse)
		}
	} else if(!is.null(percentage)) {
		stopifnot(is.numeric(percentage))
		if(length(percentage) != 1 || percentage > 1 || percentage < 0) {
			stop("percentage should be a numeric vector of length 1 with a value between 0 and 1")
		} else{
			filter_resource_percentage(log, percentage, reverse)
		}
	} else {
		stop("No filter arguments were provided. Please provide percentage or interval.")
	}
}

#' @describeIn filter_resource_frequency Filters resources for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_resource_frequency.grouped_log <- function(log,
												  interval = NULL,
												  percentage = NULL,
												  reverse = FALSE) {

	apply_grouped_fun(log, fun = filter_resource_frequency.log, interval, percentage, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_resource_frequency, interval, percentage, reverse, ...)
}

filter_resource_interval <- function(eventlog, lower, upper, reverse) {
	lower <- ifelse(is.na(lower), -Inf, lower)
	upper <- ifelse(is.na(upper), Inf, upper)

	absolute_frequency <- NULL

	event_selection <- eventlog %>%
		resources %>%
		filter(between(absolute_frequency, lower, upper)) %>%
		pull(1)

	filter_resource(eventlog, event_selection, reverse)
}

filter_resource_percentage <- function(eventlog, percentage, reverse) {
	r <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL

	resources(eventlog) %>%
		arrange(-absolute_frequency) %>%
		mutate(r = cumsum(relative_frequency)) %>%
		filter(dplyr::lag(r, default = 0) < percentage) %>%
		pull(1) -> event_selection

	filter_resource(eventlog, event_selection, reverse)
}

#' @describeIn filter_resource_frequency Filter interactively
#' @export ifilter_resource_frequency
ifilter_resource_frequency <- function(log) {

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter resources based on frequency"),
		miniContentPanel(
			fillCol(flex = c(2,1),
					fillRow(flex = c(10,1,8),
							radioButtons("filter_type", "Filter type:", choices = c("Interval" = "int", "Use percentile cutoff" = "percentile")),
							" ",
							radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
					),
					uiOutput("filter_ui")

			)
		)
	)

	server <- function(input, output, session){
		max <- max(log %>% resources() %>% pull(absolute_frequency))
		output$filter_ui <- renderUI({
			absolute_frequency <- NULL
			if(input$filter_type == "int") {
				sliderInput("interval_slider", "Process time interval",
							min = 0, max = max, value = c(0, max))

			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentage", min = 0, max = 100, value = 80)
			}
		})

		observeEvent(input$done, {



			if(input$filter_type == "int")

				fun_call <- construct_call(input_cmd, list("interval" = list(input$interval_slider), "reverse" =list(input$reverse == "Yes", FALSE)))

			else if(input$filter_type == "percentile") {
				fun_call <- construct_call(input_cmd, list("percentage" = list(input$percentile_slider/100), "reverse" = list(input$reverse == "Yes", FALSE)))
			}
			rstudioapi::sendToConsole(fun_call)
			stopApp()
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter resources based on frequency", height = 400))

}
