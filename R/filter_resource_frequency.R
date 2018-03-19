#' @title {Filter: Activity frequency}
#'
#' @description {Filters the log based on frequency of activities}
#'
#'
#' Filtering the event log based in resource frequency can be done in two ways: using an interval of allowed frequencies, or specify a coverage percentage.
#'
#' \itemize{
#'
#' \item percentage: When filtering using a percentage p\%, the filter will return p% of the activity instances, starting from the resource labels with the highest
#' frequency. The filter will retain additional resource labels as long as the number of activity instances does not exceed the percentage threshold.
#'
#' \item interval: When filtering using an interval, resource labels will be retained when their absolute frequency fall in this interval. The interval is specified using
#' a numeric vector of length 2. Half open intervals can be created by using NA. E.g., `c(10, NA)` will select resource labels which occur 10 times or more.
#' }
#'
#' @param percentage The target coverage of activity instances. A percentile of 0.9 will return the most common resource types of the eventlog,
#' which account for at least 90\% of the activity instances.
#'
#' @param interval An resource frequency interval (numeric vector of length 2). Half open interval can be created using NA.
#' @inherit filter_activity params references seealso return
#'
#' @export filter_resource_frequency

filter_resource_frequency <- function(eventlog, interval, percentage, reverse, ...) {
	UseMethod("filter_resource_frequency")
}

#' @describeIn filter_resource_frequency Filter event log
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
		pull(1) -> event_selection

	filter_resource(eventlog, event_selection, reverse)
}

#' @describeIn filter_resource_frequency Filter grouped event logs
#' @export

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
