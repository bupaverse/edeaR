#' @title Filter: Resource frequency
#'
#' @description Filters the log based on its most frequent resources, until a specific percentile cut off.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param percentile_cut_off The target coverage of events
#' A percentile of 0.9 will return the most common resource types of the eventlog, which account for 90\% of the events.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_resource_frequency

filter_resource_frequency <- function(eventlog, interval, percentage, reverse, ...) {
	UseMethod("filter_resource_frequency")
}
#'
#'
#' @describeIn filter_resource_frequency Filter event logs
#' @export

filter_resource_frequency.eventlog <- function(eventlog,
									  interval = NULL,
									  percentage = NULL,
									  reverse = FALSE,
									  ...) {

	percentage <- deprecated_perc(percentage, ...)
	stopifnot(is.logical(reverse))

	mapping <- mapping(eventlog)

	if(!is.null(interval) && !is.null(percentage)) {
		stop("Provide interval OR percentage Cannot filter using both methods.")
	} else if(!is.null(interval)) {
		stopifnot(is.numeric(interval))
		if(length(interval) != 2 || any(interval < 0, na.rm = T) || all(is.na(interval))){
			stop("Interval should be a positive numeric vector of length 2. One of the elements can be NA to create open intervals.")
		} else {
			filter_resource_interval(eventlog, interval[1], interval[2], reverse)
		}
	} else if(!is.null(percentage)) {
		stopifnot(is.numeric(percentage))
		if(length(percentage) != 1 || percentage > 1 || percentage < 0) {
			stop("percentage should be a numeric vector of length 1 with a value between 0 and 1")
		} else{
			filter_resource_percentage(eventlog, percentage, reverse)
		}
	} else {
		stop("No filter arguments were provided. Please provide percentage or interval.")
	}
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
		pull() -> event_selection

	filter_resource(eventlog, event_selection, reverse)
}

#' @describeIn filter_resource_frequency Filter grouped event logs
#' @export
#'
filter_resource_frequency.grouped_eventlog <- function(eventlog, interval = NULL, percentage = NULL, reverse = FALSE, ...) {
	grouped_filter(eventlog, filter_resource_frequency, interval, percentage, reverse, ...)
}


#' @rdname filter_resource_frequency
#' @export ifilter_resource_frequency
ifilter_resource_frequency <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter resources based on frequency"),
		miniContentPanel(
			fillCol(flex = c(2,1),
					fillRow(flex = c(10,1,8),
							sliderInput("percentile_cut_off", "Cumulative Percentile Cut-off", 0, 100, value = 80),
							" ",
							radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
					),
					"A percentile of 0.9 will return the most common resources of the eventlog, which account for 90% of the events."
			))
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_resource_frequency(eventlog,
													  percentile_cut_off = input$percentile_cut_off/100,
													  reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter resources based on frequency", height = 400))

}
