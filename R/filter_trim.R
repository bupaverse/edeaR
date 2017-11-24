#' @title Filter: Trim cases
#'
#' @description Trim all cases from the first event of a set of start activities to the last event of a set of end activities.
#' Traces that don't have at least one event of both sets are discarded.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param start_activities Start activities used for trimming. If not provided, the start of the cases is not trimmed.
#'
#' @param end_activities End activities used for trimming. If not provided, the end of the cases or not trimmed.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_trim

filter_trim <- function(eventlog, start_activities, end_activities, reverse) {
	UseMethod("filter_trim")
}

#' @describeIn filter_trim Filter event log
#' @export

filter_trim.eventlog <- function(eventlog,
						start_activities = NULL,
						end_activities = NULL,
						reverse = FALSE) {

	if(is.null(start_activities) & is.null(end_activities))
		stop("At least on start or end activity should be provided")

	acts <- activities(eventlog) %>% pull(1)

	min_timestamp <- NULL
	start_r <- NULL
	end_r <- NULL
	min_rank <- NULL
	max_rank <- NULL
	r <- NULL

	if(is.null(start_activities))
		start_activities <- acts
	if(is.null(end_activities))
		end_activities <- acts

	eventlog %>%
		group_by(!!case_id_(eventlog), !!activity_instance_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize(min_timestamp = min(!!timestamp_(eventlog))) %>%
		group_by(!!case_id_(eventlog)) %>%
		mutate(r = dense_rank(min_timestamp)) %>%
		mutate(start_r = ifelse((!!activity_id_(eventlog)) %in% start_activities, r, NA),
			   end_r = ifelse((!!activity_id_(eventlog)) %in% end_activities, r, NA)) %>%
		mutate(min_rank = min(c(Inf,start_r), na.rm = T)) %>%
		mutate(max_rank = max(c(-Inf,end_r), na.rm = T)) %>%
		filter( r >= min_rank, r <= max_rank) %>%
		pull(!!activity_instance_id_(eventlog)) -> aid_selection

	if(reverse == F)
		filter(eventlog, (!!activity_instance_id_(eventlog)) %in% aid_selection)
	else
		filter(eventlog, !((!!activity_instance_id_(eventlog)) %in% aid_selection))

}
#' @describeIn filter_trim Filter grouped event log
#' @export

filter_trim.grouped_eventlog <- function(eventlog,
								 start_activities = NULL,
								 end_activities = NULL,
								 reverse = FALSE) {
	grouped_filter(eventlog, filter_trim, start_activities, end_activities, reverse)
}

#' @rdname filter_trim
#' @export ifilter_trim
#'
#'
ifilter_trim <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Trim cases"),
		miniContentPanel(
			fillCol(flex = c(5,3,2),
					fillRow(flex = c(10,1,10),
							selectizeInput("start", label = "Select start activities:",
										   choices = eventlog %>% pull(!!activity_id_(eventlog)) %>%
										   	unique %>% sort, selected = NA,  multiple = T), " ",
							selectizeInput("end", label = "Select end activities:",
										   choices = eventlog %>% pull(!!activity_id_(eventlog)) %>%
										   	unique %>% sort, selected = NA,  multiple = T)),
					fillRow(
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")),
					"Trim all cases from the first event of a set of start activities to the last event of a set of end activities. Traces that do not have at least one event of both sets are discarded."			)

		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_trim(eventlog,
											  start_activities = input$start,
											  end_activities = input$end,
											  reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Trim cases", height = 400))

}


