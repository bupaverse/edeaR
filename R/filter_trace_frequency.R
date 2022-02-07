#' Filter: Trace frequency
#'
#' Filters the log based the frequency of traces, using an interval or a percentile cut off.
#'
#' This filter can be used to filter cases based on the frequency of the corresponding trace.
#' A trace is a sequence of activity labels, and will be discussed in
#' more detail in Section 6. There are again two ways to select cases based on trace frequency,
#' by interval or by percentile cut off. The percentile cut off will start with the most frequent
#' traces.
#'
#' @param percentage When given a percentage p, the filter will select the most common traces, until at least p\% of the cases is covered.
#'
#' @param interval WHen given an interval, the filter will select cases of which the trace has a frequency inside the interval.
#'
#'
#' @inherit filter_activity params references seealso return
#' @export filter_trace_frequency


filter_trace_frequency <- function(eventlog,
								   interval = NULL,
								   percentage = NULL,
								   reverse = FALSE,
								   ...) {
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
		absolute_frequency <- NULL
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
