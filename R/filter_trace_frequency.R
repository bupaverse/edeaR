#' @title Filter: Trace frequency percentile
#'
#' @description Filters the log based the frequency of traces, using an upper and lower threshold or a percentile cut off.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param lower_threshold The lower frequency threshold.
#' When \code{reverse} is FALSE, all traces with a lower frequency are discarded.
#'
#' @param upper_threshold The upper frequency threshold.
#' When \code{reverse} is FALSE, all traces with a higher frequency are discarded.
#'
#' @param percentile_cut_off Alternatively to providing thresholds, a percentile cut off can be provided.
#' A percentile cut off value of 0.9 will return the most common traces, accounting for 90\% of the cases.
#' When \code{reverse} is set to TRUE, it will return the least common traces, acoounting for 10\% of the cases.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#'
#' @export filter_trace_frequency


filter_trace_frequency <- function(eventlog, interval, percentage, reverse, ...) {
	UseMethod("filter_trace_frequency")
}


#' @describeIn filter_trace_frequency Filter event log
#' @export

filter_trace_frequency.eventlog <- function(eventlog,
								   interval = NULL,
								   percentage = NULL,
								   reverse = FALSE,
								   ...) {

	percentage  <- deprecated_perc(percentage, ...)
	interval[1] <- deprecated_lower_thr(interval[1], ...)
	interval[2] <- deprecated_upper_thr(interval[2], ...)


	if(!is.null(interval) && (length(interval) != 2 || !is.numeric(interval) || any(interval < 0, na.rm = T) || all(is.na(interval)) )) {
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
		filter_trace_frequency_percentile(eventlog,
										  percentage = percentage,
										  reverse = reverse)
	else
		filter_trace_frequency_threshold(eventlog,
										 lower_threshold = interval[1],
										 upper_threshold = interval[2],
										 reverse = reverse)

}

#' @describeIn filter_trace_frequency Filter grouped event log
#' @export

filter_trace_frequency.grouped_eventlog <- function(eventlog,
											interval = NULL,
											percentage = NULL,
											reverse = FALSE,
											...) {
	grouped_filter(eventlog, filter_trace_frequency, interval, percentage, reverse)
}

#' @rdname filter_trace_frequency
#' @export ifilter_trace_frequency

ifilter_trace_frequency <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter on Trace Frequency"),
		miniContentPanel(
			fillCol(
				fillRow(
					radioButtons("filter_type", "Filter type:", choices = c("Interval" = "int", "Use percentile cutoff" = "percentile")),
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
				),
				uiOutput("filter_ui")
			)
		)
	)

	server <- function(input, output, session){

		output$filter_ui <- renderUI({
			if(input$filter_type == "int") {
				sliderInput("interval_slider", "Trace frequency interval",
							min = 1, max = max(eventlog %>% trace_list() %>% pull(absolute_frequency)), value = c(1,10), step = 1)

			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentile cut off:", min = 0, max = 100, value = 80)
			}
		})

		observeEvent(input$done, {
			if(input$filter_type == "int")
				filtered_log <- filter_trace_frequency(eventlog,
													   interval = input$interval_slider,
													   reverse = ifelse(input$reverse == "Yes", T, F))
			else if(input$filter_type == "percentile") {
				filtered_log <- filter_trace_frequency(eventlog,
													   percentage = input$percentile_slider/100,
													   reverse = ifelse(input$reverse == "Yes", T, F))
			}

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter on Trace Frequency", height = 400))

}
