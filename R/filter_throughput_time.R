#' @title Filter Throughput Time
#'
#' @description Filters cases based on their [`throughput_time`].
#'
#' This filter can be used by using an `interval` or by using a `percentage`.
#' The percentage will always start with the shortest cases first and stop
#' including cases when the specified percentile is reached. On the other hand, an absolute
#' interval can be defined instead to filter cases which have a throughput time in this interval. The time units
#' in which this interval is defined can be supplied with the `units` argument.
#'
#' @inherit filter_activity params references seealso return
#' @inherit filter_processing_time params
#'
#' @seealso [`throughput_time()`],[`difftime()`]
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_throughput_time
filter_throughput_time <- function(log,
								   interval = NULL,
								   percentage = NULL,
								   reverse = FALSE,
								   units = c("secs", "mins", "hours", "days", "weeks")) {
	UseMethod("filter_throughput_time")
}

#' @describeIn filter_throughput_time Filters cases for a [`log`][`bupaR::log`].
#' @export
filter_throughput_time.log <- function(log,
									   interval = NULL,
									   percentage = NULL,
									   reverse = FALSE,
									   units = c("secs", "mins", "hours", "days", "weeks")) {

	units <- rlang::arg_match(units)

	check_interval_percentage_args(interval, percentage)

	if(!is.null(percentage))
		filter_throughput_time_percentile(log,
										  percentage = percentage,
										  reverse = reverse)
	else
		filter_throughput_time_threshold(log,
										 lower_threshold = interval[1],
										 upper_threshold = interval[2],
										 reverse = reverse,
										 units = units)
}

#' @describeIn filter_throughput_time Filters cases for a [`grouped_log`][`bupaR::grouped_log`].
#' @export
filter_throughput_time.grouped_log <- function(log,
											   interval = NULL,
											   percentage = NULL,
											   reverse = FALSE,
											   units = c("secs", "mins", "hours", "days", "weeks")) {

	bupaR:::apply_grouped_fun(log, fun = filter_throughput_time.log, interval, percentage, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_throughput_time, interval, percentage, reverse, units, ...)
}

#' @describeIn filter_throughput_time Filter interactively
#' @export ifilter_throughput_time
ifilter_throughput_time <- function(log) {

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter Througput Time"),
		miniContentPanel(
			fillCol(
				fillRow(
					radioButtons("filter_type", "Filter type:", choices = c("Interval" = "int", "Use percentile cutoff" = "percentile")),
					radioButtons("units", "Time units: ", choices = c("secs", "mins", "hours", "days", "weeks"), selected = "secs"),
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
				),
				uiOutput("filter_ui")
			)
		)
	)

	server <- function(input, output, session){
		my_max <- reactive({round(as.double(max(log %>% throughput_time("case", units = input$units) %>% pull(throughput_time))),2)})

		output$filter_ui <- renderUI({
			if(input$filter_type == "int") {
				sliderInput("interval_slider", "Throughput time interval",
							min = 0, max = my_max(), value = c(0,my_max()))

			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentile cut off:", min = 0, max = 100, value = 80)
			}
		})

		observeEvent(input$done, {
			if(input$filter_type == "int")

				fun_call <- construct_call(input_cmd, list(interval = list(input$interval_slider),
														   units = list(input$units, "'secs'"),
														   reverse = list(input$reverse == "Yes", FALSE)))

			else if(input$filter_type == "percentile") {

				fun_call <- construct_call(input_cmd, list(percentage = list(input$percentile_slider/100),
														   reverse = list(input$reverse == "Yes", FALSE)))

			}

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Throughput Time", height = 400))

}

