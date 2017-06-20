#' @title Filter: Throughput Time
#'
#' @description Filters cases based on their throughput time.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param lower_threshold The lower duration threshold, specified in number of days.
#' When \code{reverse} is FALSE, all cases with a lower duration are discarded.
#'
#' @param upper_threshold The upper duration threshold, specified in number of days.
#' When \code{reverse} is FALSE, all cases with a higher duration are discarded.
#'
#' @param percentile_cut_off Alternatively to providing thresholds, a percentile cut off can be provided.
#' A percentile cut off value of 0.9 will return the 90\% shortest cases.
#' When \code{reverse} is set to TRUE, it will return the 10\% longest cases.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @param units The time unit used for filtering.

#'
#' @export filter_throughput_time
#'

filter_throughput_time <- function(eventlog,
								   lower_threshold = NULL,
								   upper_threshold = NULL,
								   percentile_cut_off = NULL,
								   reverse = F,
								   units = "days") {

	stop_eventlog(eventlog)

	if(is.null(lower_threshold) & is.null(upper_threshold) & is.null(percentile_cut_off))
		stop("At least one threshold or a percentile cut off must be provided.")


	if((!is.null(lower_threshold) & !is.null(percentile_cut_off)) | (!is.null(upper_threshold) & !is.null(percentile_cut_off)))
		stop("Cannot filter on both thresholds and percentile cut off simultaneously.")


	if(!is.null(percentile_cut_off))
		return(filter_throughput_time_percentile(eventlog,
												 percentile_cut_off = percentile_cut_off,
												 reverse = reverse ))
	else
		return(filter_throughput_time_threshold(eventlog,
												lower_threshold = lower_threshold,
												upper_threshold = upper_threshold,
												reverse = reverse,
												units = units))
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
													   lower_threshold = input$interval_slider[1],
													   upper_threshold = input$interval_slider[2],
													   reverse = ifelse(input$reverse == "Yes", T, F),
													   units = input$units)
			else if(input$filter_type == "percentile") {
				filtered_log <- filter_throughput_time(eventlog,
													   percentile_cut_off = input$percentile_slider/100,
													   reverse = ifelse(input$reverse == "Yes", T, F),
													   units = input$units)
			}

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Througput Time", height = 400))

}

