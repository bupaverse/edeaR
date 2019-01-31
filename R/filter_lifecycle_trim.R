#' Filter: Trim activity instances based on life cycle labels
#'
#' Trim activity instances from the first event of a set of start life cycle labels to the last event of a set of end life cycle labels
#'
#' One can trim activity instances  by removing one or more events at the start and/or end of the activity instances.
#' Trimming is performed until all activity instances have a start and/or
#' end point belonging to a set of allowed life cycle labels. This filter requires a set of allowed
#' start life cycle labels and/or a set of allowed life cycle labels. If one of them is not provided it will
#' not trim the activity instances at this edge.The selection can be reversed, which means that
#' only the trimmed events at the start and end of activity instances are retained. As such, this argument
#' allows to cut intermediate parts out of activity instances.
#'
#' @param start_lifecycle A vector of life cycle identifiers, or NULL
#'
#' @param end_lifecycle A vector of life cycle identifiers, or NULL
#'
#' @inherit filter_activity params references seealso return
#' @inherit filter_endpoints params
#'
#' @export filter_trim_lifecycle

filter_trim_lifecycle <- function(eventlog,
						start_lifecycle,
						end_lifecycle,
						reverse) {
	UseMethod("filter_trim_lifecycle")
}

#' @describeIn filter_trim_lifecycle Filter event log
#' @export

filter_trim_lifecycle.eventlog <- function(eventlog,
								 start_lifecycle = NULL,
								 end_lifecycle = NULL,
								 reverse = FALSE) {
	UNIQUE_EVENT_ID <- NULL

	if(is.null(start_activities) & is.null(end_activities))
		stop("At least on start or end activity should be provided")

	acts <- (eventlog) %>% pull(!!lifecycle_id_(eventlog)) %>% unique()

	min_order <- NULL
	min_timestamp <- NULL
	start_r <- NULL
	end_r <- NULL
	min_rank <- NULL
	max_rank <- NULL
	r <- NULL
	if(is.null(start_lifecycle))
		start_lifecycle <- acts
	if(is.null(end_lifecycle))
		end_lifecycle <- acts

	eventlog %>%
		mutate(UNIQUE_EVENT_ID = 1:n()) -> eventlog

	eventlog %>%
		filter_lifecycle_presence(start_lifecycle, method = "one_of") %>%
		filter_lifecycle_presence(end_lifecycle, method = "one_of") %>%
		group_by(!!activity_instance_id_(eventlog)) %>%
		arrange(!!timestamp_(eventlog), .order) %>%
		mutate(r = 1:n()) %>%
		mutate(start_r = ifelse((!!lifecycle_id_(eventlog)) %in% start_lifecycle, r, NA),
			   end_r = ifelse((!!lifecycle_id_(eventlog)) %in% end_lifecycle, r, NA)) %>%
		mutate(min_rank = min(c(Inf,start_r), na.rm = T)) %>%
		mutate(max_rank = max(c(-Inf,end_r), na.rm = T)) %>%
		filter( r >= min_rank, r <= max_rank) %>%
		pull(UNIQUE_EVENT_ID) -> selection

	if(reverse == F)
		filter(eventlog, UNIQUE_EVENT_ID %in% selection) %>% select(-UNIQUE_EVENT_ID)
	else
		filter(eventlog, UNIQUE_EVENT_ID %in% selection) %>% select(-UNIQUE_EVENT_ID)

}
#' @describeIn filter_trim_lifecycle Filter grouped event log
#' @export

filter_trim_lifecycle.grouped_eventlog <- function(eventlog,
												   start_lifecycle = NULL,
												   end_lifecycle = NULL,
										 reverse = FALSE) {

	grouped_filter(eventlog,
				   filter_trim_lifecycle,
				   start_lifecycle,
				   end_lifecycle,
				   reverse)
}

#' @rdname filter_trim_lifecycle
#' @export ifilter_trim_lifecycle
#'
#'
ifilter_trim_lifecycle <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Trim cases"),
		miniContentPanel(
			fillCol(flex = c(5,3,2),
					fillRow(flex = c(10,1,10),
							selectizeInput("start", label = "Select start life cycle labels:",
										   choices = eventlog %>% pull(!!lifecycle_id_(eventlog)) %>%
										   	unique %>% sort, selected = NA,  multiple = T), " ",
							selectizeInput("end", label = "Select end life cycle labels:",
										   choices = eventlog %>% pull(!!lifecycle_id_(eventlog)) %>%
										   	unique %>% sort, selected = NA,  multiple = T)),
					fillRow(
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")),
					"Trim all activity instances from the first event of a set of start life cycle labels to the last event of a set of end life cycle labels. Activity instances that do not have at least one event of both sets are discarded."			)

		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_trim_lifecycle(eventlog,
												  start_lifecycle = input$start,
												  end_lifecycle = input$end,
										reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Trim cases", height = 400))

}


