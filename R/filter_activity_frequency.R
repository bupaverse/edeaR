#' @title Filter Activity Frequency
#'
#' @description Filters the log based on frequency of activities.
#'
#' @param percentage,interval The target coverage of activity instances. Provide either \code{percentage} or \code{interval}.\cr
#' \code{percentage} (\code{\link{numeric}}): A percentile of p will return the most common activity types of the log,
#' which account for at least p% of the activity instances.\cr
#' \code{interval} (\code{\link{numeric}} vector of length 2): An activity frequency interval. Half open interval can be created using \code{\link{NA}}.\cr
#' For more information, see 'Details' below.
#'
#' @details
#' Filtering the log based on activity frequency can be done in two ways: using an \code{interval} of allowed frequencies,
#' or specify a coverage \code{percentage}:
#'
#' \itemize{
#' \item \code{percentage}: When filtering using a percentage p%, the filter will return p% of the activity instances,
#' starting from the activity labels with the highest frequency. The filter will retain additional activity labels
#' as long as the number of activity instances does not exceed the percentage threshold.
#' \item \code{interval}: When filtering using an interval, activity labels will be retained when their absolute frequency fall in this interval.
#' The interval is specified using a numeric vector of length 2. Half open intervals can be created by using \code{\link{NA}},
#' e.g., \code{c(10, NA)} will select activity labels which occur 10 times or more.
#' }
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @export filter_activity_frequency
filter_activity_frequency <- function(log,
									  interval = NULL,
									  percentage = NULL,
									  reverse = FALSE,
                       				  eventlog = deprecated()) {
	UseMethod("filter_activity_frequency")
}

#' @describeIn filter_activity_frequency Filters activities for a \code{\link[bupaR]{log}}.
#' @export
filter_activity_frequency.log <- function(log,
										  interval = NULL,
										  percentage = NULL,
										  reverse = FALSE,
										  eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_activity_frequency(eventlog)",
			with = "filter_activity_frequency(log)")
		log <- eventlog
	}

	stopifnot(is.logical(reverse))

	if(!is.null(interval) && !is.null(percentage)) {
		stop("Provide interval OR percentage. Cannot filter using both methods.")
	} else if(!is.null(interval)) {
		stopifnot(is.numeric(interval))
		if(length(interval) != 2 || any(interval < 0, na.rm = T) || all(is.na(interval))){
			stop("Interval should be a positive numeric vector of length 2. One of the elements can be NA to create open intervals.")
		} else {
			filter_activity_interval(log, interval[1], interval[2], reverse)
		}
	} else if(!is.null(percentage)) {
		stopifnot(is.numeric(percentage))
		if(length(percentage) != 1 || percentage > 1 || percentage < 0) {
			stop("percentage should be a numeric vector of length 1 with a value between 0 and 1")
		} else{
			filter_activity_percentage(log, percentage, reverse)
		}
	} else {
		stop("No filter arguments were provided. Please provide percentage or interval.")
	}
}

#' @describeIn filter_activity_frequency Filters activities for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_activity_frequency.grouped_log <- function(log,
												  interval = NULL,
												  percentage = NULL,
												  reverse = FALSE,
												  eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	bupaR:::apply_grouped_fun(log, fun = filter_activity_frequency.log, interval, percentage, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

filter_activity_interval <- function(log, lower, upper, reverse) {
	absolute_frequency <- NULL
	lower <- ifelse(is.na(lower), -Inf, lower)
	upper <- ifelse(is.na(upper), Inf, upper)

	activities(log) %>%
		filter(between(absolute_frequency, lower, upper)) %>%
		pull(1) -> event_selection

	filter_activity(log, event_selection, reverse)
}


filter_activity_percentage <- function(log, percentage, reverse) {
	absolute_frequency <- NULL
	relative_frequency <- NULL
	r <- NULL

	act_freq <- activities(log) %>%
		arrange(-absolute_frequency) %>%
		mutate(r = cumsum(relative_frequency)) %>%
		filter(dplyr::lag(r, default = 0) < percentage) %>%
		dplyr::pull(1) -> event_selection

	filter_activity(log, event_selection, reverse)

}


#' @export ifilter_activity_frequency
ifilter_activity_frequency <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_activity_frequency()")

	ui <- miniPage(
		gadgetTitleBar("Filter activities based on frequency"),
		miniContentPanel(
			fillCol(flex = c(2,1),
					fillRow(flex = c(10,1,8),
							radioButtons("filter_type", "Filter type:", choices = c("Interval" = "int", "Use percentile cutoff" = "percentile")),
							" ",
							radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
					),
					uiOutput("filter_ui")

			)
		)
	)

	server <- function(input, output, session){

		output$filter_ui <- renderUI({
			absolute_frequency <- NULL
			if(input$filter_type == "int") {
				sliderInput("interval_slider", "Process time interval",
							min = 0, max = max(eventlog %>% activities() %>% pull(absolute_frequency)), value = c(0,1))

			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentage", min = 0, max = 100, value = 80)
			}
		})

		observeEvent(input$done, {

			if(input$filter_type == "int")
				filtered_log <- filter_activity_frequency(eventlog,
														  interval =  input$interval_slider,
														  reverse = ifelse(input$reverse == "Yes", TRUE, FALSE))
			else if(input$filter_type == "percentile") {
				filtered_log <- filter_activity_frequency(eventlog,
														  percentage = input$percentile_slider/100,
														  reverse = ifelse(input$reverse == "Yes", TRUE, FALSE))
			}

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter activities based on frequency", height = 400))

}
