#' @title Filter: Time Period
#' @description Function to filter eventlog using a time period.
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#' @param start_point Start timestamp of the time period. This should be a date object.
#' @param end_point End timestamp of the time period. This should be a data object.
#' @param filter_method Can be \code{contained, start, complete, intersecting} or \code{trim}.
#' \code{contained} keeps all the events related to cases contained in the time period.
#' \code{start} keeps all the events related to cases started in the time period.
#' \code{complete} keeps all the events related to cases complete in the time period.
#' \code{intersecting} keeps all the events related to cases in which at least one event started and/or ended in the time period.
#' \code{trim} keeps all the events which started and ended in the time frame.
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#' @export

filter_time_period <- function(eventlog,
							   start_point,
							   end_point,
							   filter_method = c("contained","intersecting","start","complete","trim"),
							   reverse = FALSE)
{
	stop_eventlog(eventlog)

	filter_method <- match.arg(filter_method)


	if(!any(c("POSIXct", "Date") %in% class(start_point))) {
		stop("Start_point should be a date object.")
	}
	if(!any(c("POSIXct", "Date") %in% class(end_point))) {
		stop("End_point should be a date object.")
	}

	c_sum <- cases(eventlog = eventlog)
	colnames(c_sum)[colnames(c_sum)==case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog)==case_id(eventlog)] <- "case_classifier"


	if(filter_method == "contained") {
		case_selection <- filter(c_sum, start_timestamp >= start_point, complete_timestamp <= end_point) %>%
			select(case_classifier)
	}
	else if(filter_method == "intersecting") {
		case_selection <- filter(c_sum, start_timestamp >= start_point & start_timestamp <= end_point |
								 	complete_timestamp >= start_point & complete_timestamp <= end_point |
								 	start_timestamp <= start_point & complete_timestamp >= end_point) %>%
			select(case_classifier)
	}
	else if(filter_method == "start") {
		case_selection <- filter(c_sum, start_timestamp >= start_point, start_timestamp <= end_point) %>%
			select(case_classifier)
	}
	else if(filter_method == "complete") {
		case_selection <- filter(c_sum, complete_timestamp >= start_point, complete_timestamp <= end_point) %>%
			select(case_classifier)
	}
	else if(filter_method == "trim") {



		colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"
		colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
		colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
		colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"

		e <- eventlog %>%
			group_by(case_classifier, event_classifier, activity_instance_classifier) %>%
			summarize(start = min(timestamp_classifier), complete = max(timestamp_classifier))

		if(reverse == FALSE)
			f_eventlog <- filter(e, start >= start_point &
								 	complete <= end_point)
		else
			f_eventlog <- filter(e, !(start >= start_point &
									  	complete <= end_point))


		output <- filter(eventlog, activity_instance_classifier %in% f_eventlog$activity_instance_classifier)

		colnames(output)[colnames(output) == "case_classifier"] <- case_id(eventlog)
		colnames(output)[colnames(output) == "event_classifier"] <- activity_id(eventlog)
		colnames(output)[colnames(output) == "timestamp_classifier"] <- timestamp(eventlog)
		colnames(output)[colnames(output) == "activity_instance_classifier"] <- activity_instance_id(eventlog)

		output %>% re_map(mapping(eventlog)) %>% return()
	}


	if(reverse == FALSE)
		f_eventlog <- filter(eventlog, (case_classifier %in% case_selection$case_classifier))
	else
		f_eventlog <- filter(eventlog, !(case_classifier %in% case_selection$case_classifier))

	colnames(f_eventlog)[colnames(f_eventlog) == "case_classifier"] <- case_id(eventlog)

	f_eventlog %>% re_map(mapping(eventlog))  %>% return()
}



#' @rdname filter_time_period
#' @export ifilter_time_period
ifilter_time_period <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter time period"),
		miniContentPanel(
			fillRow(flex = c(3,3,3),
					fillCol(
						dateInput("start_date", "Start Date", value = as.Date(min(eventlog %>% pull(!!timestamp(eventlog))))),
						timeInput("start_time", "Start Time")
					),
					fillCol(
						dateInput("end_date", "End Date", value = as.Date(max(eventlog %>% pull(!!timestamp(eventlog))))),
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

			filtered_log <- filter_time_period(eventlog, start_point = start_date,
											   end_point = end_date,filter_method = input$method,
											   reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Time Period", height = 600))

}

