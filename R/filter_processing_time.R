#' @title Filter Processing Time
#'
#' @description Filters cases based on their \code{\link{processing_time}}.
#'
#' This filter can be used by using an \code{interval} or by using a \code{percentage}.
#' The percentage will always start with the shortest cases first and stop
#' including cases when the specified percentile is reached. On the other hand, an absolute
#' interval can be defined instead to filter cases which have a processing time in this interval. The time units
#' in which this interval is defined can be supplied with the \code{units} argument.
#'
#' @param interval,percentage Provide either \code{interval} or \code{percentage}.\cr
#' \code{interval} (\code{\link{numeric}} vector of length 2): A duration interval. Half open interval can be created using \code{\link{NA}}.\cr
#' \code{percentage} (\code{\link{numeric}}): A percentage p to be used for relative filtering.
#'
#' @inherit filter_activity params references seealso return
#' @inherit processing_time params
#'
#' @seealso \code{\link{processing_time}}
#'
#' @family filters
#'
#' @export filter_processing_time
filter_processing_time <- function(log,
								   interval = NULL,
								   percentage = NULL,
								   reverse = FALSE,
								   units = c("auto", "secs", "mins", "hours", "days", "weeks"),
								   eventlog = deprecated()) {
	UseMethod("filter_processing_time")
}

#' @describeIn filter_processing_time Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_processing_time.log <- function(log,
									   interval = NULL,
									   percentage = NULL,
									   reverse = FALSE,
									   units = c("auto", "secs", "mins", "hours", "days", "weeks"),
									   eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_processing_time(eventlog)",
			with = "filter_processing_time(log)")
		log <- eventlog
	}

	units <- rlang::arg_match(units)

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
		filter_processing_time_percentile(log,
										  percentage = percentage,
										  reverse = reverse)
	else
		filter_processing_time_threshold(log,
										 lower_threshold = interval[1],
										 upper_threshold = interval[2],
										 reverse = reverse,
										 units = units)
}

#' @describeIn filter_processing_time Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_processing_time.grouped_log <- function(log,
											   interval = NULL,
											   percentage = NULL,
											   reverse = FALSE,
											   units = c("auto", "secs", "mins", "hours", "days", "weeks"),
											   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, fun = filter_processing_time.log, interval, percentage, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_processing_time, interval = interval, percentage = percentage, reverse, units, ...)
}


#' @export ifilter_processing_time
ifilter_processing_time <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_processing_time()")

	ui <- miniPage(
		gadgetTitleBar("Filter Processing Time"),
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
				sliderInput("interval_slider", "Process time interval",
									min = 0, max = max(eventlog %>% processing_time("case", units = input$units) %>% pull(processing_time)), value = c(0,1))

			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentile cut off:", min = 0, max = 100, value = 80)
			}
		})

		observeEvent(input$done, {
			if(input$filter_type == "int")
				filtered_log <- filter_processing_time(eventlog,
												 interval = input$interval_slider,
												 reverse = ifelse(input$reverse == "Yes", T, F),
												 units = input$units)
			else if(input$filter_type == "percentile") {
				filtered_log <- filter_processing_time(eventlog,
												 percentage = input$percentile_slider/100,
												 reverse = ifelse(input$reverse == "Yes", T, F),
												 units = input$units)
			}

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Processing Time", height = 400))

}
