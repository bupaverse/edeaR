#' @title Filter Time Period
#'
#' @description Function to filter the log using a time period.
#'
#' @param interval \code{\link[base]{Date}} or \code{\link[base]{POSIXct}} vector: A time interval (vector of length 2
#' of type \code{\link[base]{Date}} or \code{\link[base]{POSIXct}}). Half-open intervals can be created with \code{\link{NA}}.
#' @param filter_method \code{\link{character}} (default \code{"contained"}): Filtering method: \code{"contained"} (default),
#' \code{"intersecting"}, \code{"start"}, \code{"complete"}, or \code{"trim"}. For more information, see 'Details' below.
#' @param force_trim \code{\link{logical}} (default \code{FALSE}): If \code{TRUE} in combination with \code{filter_method}
#' \code{"trim"}, activity instances on the edges of the interval are cut at the exact edge of the \code{interval}.
#'
#' @details
#' Event data can be filtered by supplying a time window to the method \code{filter_time_period}. There are 5 different
#' values for \code{filter_method}:
#'
#' \itemize{
#' \item \code{"contained"}: Keeps all the events related to cases contained in the time period.
#' \item \code{"intersecting"}: Keeps all the events related to cases in which at least one event started and/or ended in the time period.
#' \item \code{"start"}: Keeps all the events related to cases started in the time period.
#' \item \code{"complete"}: Keeps all the events related to cases complete in the time period.
#' \item \code{"trim"}: Keeps all the events which started and ended in the time frame.
#' }
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_event
#'
#' @export
filter_time_period <- function(log,
							   interval = NULL,
							   filter_method = c("contained", "intersecting", "start", "complete", "trim"),
							   force_trim = FALSE,
							   reverse = FALSE,
							   eventlog = deprecated()) {
	UseMethod("filter_time_period")
}

#' @describeIn filter_time_period Filters activity instances for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_time_period.eventlog <- function(log,
										interval = NULL,
										filter_method = c("contained", "intersecting", "start", "complete", "trim"),
										force_trim = FALSE,
										reverse = FALSE,
										eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_time_period(eventlog)",
			with = "filter_time_period(log)")
		log <- eventlog
	}

	filter_method <- rlang::arg_match(filter_method)

	#if(length(list(...)) > 0 && stringr::str_detect(names(list(...)), "start_point|end_point" ))
	#	stop("Arguments start_point and end point are deprecated. Please use interval instead.")

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
		end_point <- Inf
	} else {
		end_point <- interval[2]
	}

	start_timestamp <- NULL
	complete_timestamp <- NULL

	cases <- cases(log)

	if(filter_method == "trim") {
		aid_selection <- log %>%
			group_by_activity_instance() %>%
			summarize(start_timestamp = min(!!timestamp_(log)), complete_timestamp = max(!!timestamp_(log))) %>%
			filter((start_timestamp >= start_point & start_timestamp <= end_point) |
				   	(start_timestamp <= start_point & complete_timestamp >= end_point) |
				   	(complete_timestamp >= start_point & complete_timestamp <= end_point)) %>%
			pull(1)

		output <- filter_activity_instance(log, activity_instances = aid_selection, reverse = reverse)

		if(force_trim) {
			output %>%
				pull(!!timestamp_(log)) -> time_vector

			time_vector[time_vector < start_point] <- start_point
			time_vector[time_vector > end_point] <- end_point

			output[, timestamp(log)] <- time_vector
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

		filter_case.log(log, cases = case_selection, reverse = reverse)
	}
}

#' @describeIn filter_time_period Filters activity instances for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
filter_time_period.grouped_eventlog <- function(log,
												interval = NULL,
												filter_method = c("contained", "intersecting", "start", "complete", "trim"),
												force_trim = FALSE,
												reverse = FALSE,
												eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_time_period(eventlog)",
			with = "filter_time_period(log)")
		log <- eventlog
	}

	bupaR:::apply_grouped_fun(log, fun = filter_time_period.eventlog, interval, filter_method, force_trim, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_time_period, interval, filter_method, force_trim, reverse, ...)
}

#' @describeIn filter_time_period Filters activity instances for an \code{\link[bupaR]{activitylog}}.
#' @export
filter_time_period.activitylog <- function(log,
										   interval = NULL,
										   filter_method = c("contained", "intersecting", "start", "complete", "trim"),
										   force_trim = FALSE,
										   reverse = FALSE,
										   eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_time_period(eventlog)",
			with = "filter_time_period(log)")
		log <- eventlog
	}

	filter_time_period.eventlog(log = bupaR::to_eventlog(log), interval = interval, filter_method = filter_method, force_trim = force_trim, reverse = reverse) %>%
		bupaR::to_activitylog()
}

#' @describeIn filter_time_period Filters activity instances for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
filter_time_period.grouped_activitylog <- function(log,
												   interval = NULL,
												   filter_method = c("contained", "intersecting", "start", "complete", "trim"),
												   force_trim = FALSE,
												   reverse = FALSE,
												   eventlog = deprecated()) {

	log <- lifecycle_warning_eventlog(log, eventlog)

	filter_time_period.grouped_eventlog(log = bupaR::to_eventlog(log), interval = interval, filter_method = filter_method, force_trim = force_trim, reverse = reverse) %>%
		bupaR::to_activitylog()
}

#' @keywords internal
#' @rdname ifilter
#' @export ifilter_time_period
ifilter_time_period <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_time_period()")

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

