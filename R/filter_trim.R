#' Filter: Trim cases
#'
#' Trim cases from the first event of a set of start activities to the last event of a set of end activities.
#'
#' One can trim cases by removing one or more activity instances
#' at the start and/or end of a case. Trimming is performed until all cases have a start and/or
#' end point belonging to a set of allowed activity labels. This filter requires a set of allowed
#' start activities and/or a set of allowed end activities. If one of them is not provided it will
#' not trim the cases at this edge.The selection can be reversed, which means that
#' only the trimmed events at the start and end of cases are retained. As such, this argument
#' allows to cut intermediate parts out of traces.
#'
#'
#' @inherit filter_activity params references seealso return
#' @inherit filter_endpoints params
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

	min_order <- NULL
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
		filter_activity_presence(start_activities, method = "one_of") %>%
		filter_activity_presence(end_activities, method = "one_of") %>%
		group_by(!!case_id_(eventlog), !!activity_instance_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize(min_timestamp = min(!!timestamp_(eventlog), Inf), "min_order" = min(.order, Inf), n = n()) %>%
		as.data.frame() %>%
		filter(n > 0) %>%
		group_by(!!case_id_(eventlog)) %>%
		arrange(min_timestamp, min_order) %>%
		mutate(r = 1:n()) %>%
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


