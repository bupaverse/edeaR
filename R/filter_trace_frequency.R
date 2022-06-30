#' @title Filter Trace Frequency
#'
#' @description Filters the log based the frequency of traces, using an interval or a percentile cut off.
#'
#' @param percentage,interval The target coverage of activity instances. Provide either \code{percentage} or \code{interval}.\cr
#' \code{percentage} (\code{\link{numeric}}): A percentile of p will select the most common traces of the log,
#' until at least p% of the cases is covered.\cr
#' \code{interval} (\code{\link{numeric}} vector of length 2): A trace frequency interval. The filter will select cases
#' of which the trace has a frequency inside the interval. Half open interval can be created using \code{\link{NA}}.\cr
#' For more information, see 'Details' below.
#'
#' @details
#' Filtering the log based on trace frequency can be done in two ways: using an \code{interval} of allowed frequencies,
#' or specify a coverage \code{percentage}:
#'
#' \itemize{
#' \item \code{percentage}: When filtering using a percentage p%, the filter will return p% of the cases, starting from the traces
#' with the highest frequency. The filter will retain additional traces as long as the number of activity instances does not exceed the percentage threshold.
#' \item \code{interval}: When filtering using an interval, traces will be retained when their absolute frequency fall in this interval.
#' The interval is specified using a numeric vector of length 2. Half open intervals can be created by using \code{\link{NA}},
#' e.g., \code{c(10, NA)} will select cases with a trace that occurs 10 times or more.
#' }
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @export filter_trace_frequency
filter_trace_frequency <- function(log, interval = NULL, percentage = NULL, reverse = FALSE, eventlog = deprecated()) {
	UseMethod("filter_trace_frequency")
}


#' @describeIn filter_trace_frequency Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_trace_frequency.log <- function(log, interval = NULL, percentage = NULL, reverse = FALSE, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_trace_frequency(eventlog)",
			with = "filter_trace_frequency(log)")
		log <- eventlog
	}

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
		filter_trace_frequency_percentile(log,
										  percentage = percentage,
										  reverse = reverse)
	else
		filter_trace_frequency_threshold(log,
										 lower_threshold = interval[1],
										 upper_threshold = interval[2],
										 reverse = reverse)

}

#' @describeIn filter_trace_frequency Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_trace_frequency.grouped_log <- function(log, interval = NULL, percentage = NULL,	reverse = FALSE, eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, fun = filter_trace_frequency.log, interval, percentage, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}


#' @export ifilter_trace_frequency
ifilter_trace_frequency <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_trace_frequency()")

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
