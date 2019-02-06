#' Filter: Throughput Time
#'
#' Filters cases based on their throughput time.
#'
#' This filter can be used by using an interval or by using a percentage.
#' The percentage will always start with the shortest cases first and stop
#' including cases when the specified percentile is reached. On the other hand, an absolute
#' interval can be defined instead to filter cases which have a throughput time in this interval. The time units
#' in which this interval is defined can be submitted with the units argument.
#'
#' @inherit filter_activity params references seealso return
#' @inherit filter_processing_time params
#'
#' @export filter_throughput_time
#'

filter_throughput_time <- function(eventlog, interval, percentage, reverse, units, ...) {
	UseMethod("filter_throughput_time")
}

#' @describeIn filter_throughput_time Filter event log
#' @export

filter_throughput_time.eventlog <- function(eventlog,
								   interval = NULL,
								   percentage = NULL,
								   reverse = FALSE,
								   units = c("days","hours","mins","secs","weeks"),
								   ...) {

	units <- match.arg(units)
	percentage <- deprecated_perc(percentage, ...)
	interval[1] <- deprecated_lower_thr(interval[1], ...)
	interval[2] <- deprecated_upper_thr(interval[2], ...)


	if(!is.null(interval) && (length(interval) != 2 || !is.numeric(interval) || any(interval < 0, na.rm = TRUE) || all(is.na(interval)) )) {
		stop("Interval should be a positive numeric vector of length 2. One of the elements can be NA to create open intervals.")
	}
	if(!is.null(percentage) && (!is.numeric(percentage) || !between(percentage,0,1) )) {
		stop("Percentage should be a numeric value between 0 and 1.")
	}

	if(is.null(interval) & is.null(percentage))
		stop("At least an interval or a percentage must be provided.")
	else if((!is.null(interval)) & !is.null(percentage))
		stop("Cannot filter on both interval and percentage simultaneously.")
	else if(!is.null(percentage))
		filter_throughput_time_percentile(eventlog,
										  percentage = percentage,
										  reverse = reverse)
	else
		filter_throughput_time_threshold(eventlog,
										 lower_threshold = interval[1],
										 upper_threshold = interval[2],
										 reverse = reverse,
										 units = units)
}

#' @describeIn filter_throughput_time Filter grouped event log
#' @export

filter_throughput_time.grouped_eventlog <- function(eventlog,
											interval = NULL,
											percentage = NULL,
											reverse = FALSE,
											units = c("days","hours","mins","secs","week"),
											...) {
	grouped_filter(eventlog, filter_throughput_time, interval, percentage, reverse, units, ...)
}


#' @rdname filter_throughput_time
#' @export ifilter_throughput_time

ifilter_throughput_time <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter Througput Time"),
		miniContentPanel(
			fillCol(
				fillRow(
					radioButtons("filter_type", "Filter type:", choices = c("Interval" = "int", "Use percentile cutoff" = "percentile")),
					radioButtons("units", "Time units: ", choices = c("weeks","days","hours","mins"), selected = "hours"),
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
				),
				uiOutput("filter_ui")
			)
		)
	)

	server <- function(input, output, session){

		output$filter_ui <- renderUI({
			if(input$filter_type == "int") {
				sliderInput("interval_slider", "Throguhput time interval",
							min = 0, max = max(eventlog %>% throughput_time("case", units = input$units) %>% pull(throughput_time)), value = c(0,1))

			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentile cut off:", min = 0, max = 100, value = 80)
			}
		})

		observeEvent(input$done, {
			if(input$filter_type == "int")
				filtered_log <- filter_throughput_time(eventlog,
													   interval = input$interval_slider,
													   reverse = ifelse(input$reverse == "Yes", TRUE, FALSE),
													   units = input$units)
			else if(input$filter_type == "percentile") {
				filtered_log <- filter_throughput_time(eventlog,
													   percentage = input$percentile_slider/100,
													   reverse = ifelse(input$reverse == "Yes", TRUE, FALSE),
													   units = input$units)
			}

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Througput Time", height = 400))

}

