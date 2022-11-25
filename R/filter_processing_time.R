#' @title Filter Processing Time
#'
#' @description Filters cases based on their [`processing_time`].
#'
#' This filter can be used by using an `interval` or by using a `percentage`.
#' The percentage will always start with the shortest cases first and stop
#' including cases when the specified percentile is reached. On the other hand, an absolute
#' interval can be defined instead to filter cases which have a processing time in this interval. The time units
#' in which this interval is defined can be supplied with the `units` argument.
#'
#' @param interval,percentage Provide either `interval` or `percentage`.\cr
#' `interval` ([`numeric`] vector of length 2): A duration interval. Half open interval can be created using [`NA`].\cr
#' `percentage` ([`numeric`]): A percentage to be used for relative filtering.
#' @param units [`character`] (default `"secs"`): The time unit in which the processing times should be reported. Should be one of the following values:
#' `"secs"` (default), `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the `units` argument of [`difftime()`].
#'
#' @inherit filter_activity params references seealso return
#'
#' @seealso [`processing_time()`],[`difftime()`]
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_processing_time
filter_processing_time <- function(log,
								   interval = NULL,
								   percentage = NULL,
								   reverse = FALSE,
								   units = c("secs", "mins", "hours", "days", "weeks"),
								   eventlog = deprecated()) {
	UseMethod("filter_processing_time")
}

#' @describeIn filter_processing_time Filters cases for a [`log`][`bupaR::log`].
#' @export
filter_processing_time.log <- function(log,
									   interval = NULL,
									   percentage = NULL,
									   reverse = FALSE,
									   units = c("secs", "mins", "hours", "days", "weeks"),
									   eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_processing_time(eventlog)",
			with = "filter_processing_time(log)")
		log <- eventlog
	}

	units <- rlang::arg_match(units)

	check_interval_percentage_args(interval, percentage)

	if(!is.null(percentage))
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

#' @describeIn filter_processing_time Filters cases for a [`grouped_log`][`bupaR::grouped_log`].
#' @export
filter_processing_time.grouped_log <- function(log,
											   interval = NULL,
											   percentage = NULL,
											   reverse = FALSE,
											   units = c("secs", "mins", "hours", "days", "weeks"),
											   eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_processing_time(eventlog)",
			with = "filter_processing_time(log)")
		log <- eventlog
	}

	bupaR:::apply_grouped_fun(log, fun = filter_processing_time.log, interval, percentage, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_processing_time, interval = interval, percentage = percentage, reverse, units, ...)
}


check_interval_percentage_args <- function(interval, percentage, call = caller_env()) {

	if(!is.null(interval) && (length(interval) != 2 || !is.numeric(interval) || any(interval < 0, na.rm = TRUE) || all(is.na(interval)))) {
		cli_abort(c("{.arg interval} should be a positive {.cls numeric} vector of length 2.",
					"x" = "You supplied a {.cls {class(interval)}}: {.val {interval}}",
					"i" = "One of the elements can be {.code NA} to create open intervals."),
				  call = call)
	}

	if(!is.null(percentage) && (!is.numeric(percentage) || !between(percentage,0,1) )) {
		cli_abort(c("{.arg percentage} should be a {.cls numeric} between 0 and 1.",
					"x" = "You supplied a {.cls {class(percentage)}}: {.val {percentage}}"),
				  call = call)
	}

	if(is.null(interval) && is.null(percentage))
		cli_abort("At least an {.arg interval} or a {.arg percentage} must be provided.",
				  call = call)
	else if(!is.null(interval) && !is.null(percentage))
		cli_abort("Cannot filter on both {.arg interval} and {.arg percentage} simultaneously.",
				  call = call)
}

#' @keywords internal
#' @rdname ifilter
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
