#' Filter: Time Period
#'
#'
#' Function to filter eventlog using a time period.
#'
#'
#' Event data can be filtered by supplying a time window to the method filter_time_period. There are 5 different filter methods.
#'
#' \itemize{
#' \item \code{contained} keeps all the events related to cases contained in the time period.
#' \item \code{start} keeps all the events related to cases started in the time period.
#' \item \code{complete} keeps all the events related to cases complete in the time period.
#' \item \code{intersecting} keeps all the events related to cases in which at least one event started and/or ended in the time period.
#' \item \code{trim} keeps all the events which started and ended in the time frame.
#' }
#'
#' @param interval A time interval. A vector of length 2 of type Date or POSIXct. Half-open intervals can be created with NA.
#' @param filter_method Can be \code{contained, start, complete, intersecting} or \code{trim}.
#' @param force_trim Logical, if true in combination with filter method trim activity instances on the edges of the interval are cut at the exact edge of the interval.
#'
#' @inherit filter_activity params references seealso return
#'
#' @export


filter_time_period <- function(eventlog, interval, filter_method, force_trim, reverse, ...) {
	UseMethod("filter_time_period")
}

#' @describeIn filter_time_period Filter event log
#' @export

filter_time_period.eventlog <- function(eventlog,
										interval = NULL,
										filter_method = c("contained","intersecting","start","complete","trim"),
										force_trim = FALSE,
										reverse = FALSE,
										...) {

	filter_method <- match.arg(filter_method)

	if(length(list(...)) > 0 && stringr::str_detect(names(list(...)), "start_point|end_point" ))
		stop("Arguments start_point and end point are deprecated. Please use interval instead.")

	if(!any(c("POSIXct", "Date") %in% class(interval))) {
		stop("Start_point should be a date object.")
	} else if(length(interval) != 2) {
		stop("interval should be a vector of length 2.")
	}

	if(is.na(interval[1])) {
		start_point <- -Inf
	} else {
		start_point <- interval[1]
	}

	if(is.na(interval[2])) {
		end_point <- -Inf
	} else {
		end_point <- interval[2]
	}

	start_timestamp <- NULL
	complete_timestamp <- NULL

	cases <- cases(eventlog = eventlog)

	if(filter_method == "trim") {
		aid_selection <- eventlog %>%
			group_by_activity_instance() %>%
			summarize(start_timestamp = min(!!timestamp_(eventlog)), complete_timestamp = max(!!timestamp_(eventlog))) %>%
			filter((start_timestamp >= start_point & start_timestamp <= end_point) |
				   	(start_timestamp <= start_point & complete_timestamp >= end_point) |
				   	(complete_timestamp >= start_point & complete_timestamp <= end_point)) %>%
			pull(1)

		if(reverse == FALSE)
			output <- filter(eventlog, (!!activity_instance_id_(eventlog)) %in% aid_selection)
		else
			output <- filter(eventlog, !((!!activity_instance_id_(eventlog)) %in% aid_selection))


		if(force_trim) {
			output %>%
				pull(!!timestamp_(eventlog)) -> time_vector

			time_vector[time_vector < start_point] <- start_point
			time_vector[time_vector > end_point] <- end_point

			output[, timestamp(eventlog)] <- time_vector
			output
		} else
			output


	} else {
		if(filter_method == "contained") {
			cases %>%
				filter(start_timestamp >= start_point, complete_timestamp <= end_point) %>%
				pull(1) -> case_selection
		}
		else if(filter_method == "intersecting") {
			cases %>%
				filter((start_timestamp >= start_point & start_timestamp <= end_point) |
					   	(complete_timestamp >= start_point & complete_timestamp <= end_point) |
					   	(start_timestamp <= start_point & complete_timestamp >= end_point)) %>%
				pull(1) -> case_selection
		}
		else if(filter_method == "start") {
			cases %>%
				filter(start_timestamp >=  start_point & start_timestamp <= end_point) %>%
				pull(1) -> case_selection
		}
		else if(filter_method == "complete") {
			cases %>%
				filter(complete_timestamp >= start_point & complete_timestamp <= end_point) %>%
				pull(1) -> case_selection
		}
		filter_case(eventlog, case_selection, reverse)
	}
}

#' @describeIn filter_time_period Filter grouped event log
#' @export


filter_time_period.grouped_eventlog <- function(eventlog,
												interval = NULL,
												filter_method = c("contained","intersecting","start","complete","trim"),
												force_trim = FALSE,
												reverse = FALSE,
												...) {

	grouped_filter(eventlog, filter_time_period, interval, filter_method, force_trim, reverse, ...)

}
#' @rdname filter_time_period
#' @export ifilter_time_period
ifilter_time_period <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter time period"),
		miniContentPanel(
			fillRow(flex = c(3,3,3),
					fillCol(
						dateInput("start_date", "Start Date", value = as.Date(min(eventlog %>% pull(timestamp(eventlog))))),
						timeInput("start_time", "Start Time")
					),
					fillCol(
						dateInput("end_date", "End Date", value = as.Date(max(eventlog %>% pull(timestamp(eventlog))))),
						timeInput("end_time", "End Time")
					)
			),
			fillRow(
				radioButtons("method", "Filter method: ", choices = c("contained","intersecting","start","complete","trim"), selected = "contained"),
				radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {
			start_date <- ymd_hms(paste(as.character(input$start_date), strftime(input$start_time, "%T")))
			end_date <- ymd_hms(paste(as.character(input$end_date), strftime(input$end_time, "%T")))
			print(start_date)
			print(end_date)

			filtered_log <- filter_time_period(eventlog,
											   interval = c(start_date, end_date),
											   filter_method = input$method,
											   reverse = ifelse(input$reverse == "Yes", TRUE, FALSE))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Time Period", height = 600))
}

