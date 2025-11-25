#' @title Filter Trim Life Cycle
#'
#' @description Trim activity instances from the first event of a set of start life cycle labels to the last event of a set of end life cycle labels.
#'
#' One can trim activity instances by removing one or more events at the start and/or end of the activity instances.
#' Trimming is performed until all activity instances have a start and/or end point belonging to a set of allowed life cycle labels.
#' This filter requires a set of allowed start life cycle labels and/or a set of allowed life cycle labels. If one of them
#' is not provided it will not trim the activity instances at this edge.The selection can be reversed, which means that
#' only the trimmed events at the start and end of activity instances are retained. As such, this argument allows to cut
#' intermediate parts out of activity instances.
#'
#' @param start_lifecycles,end_lifecycles \code{\link{character}} vector (default \code{\link{NULL}}): A vector of life cycle
#' identifiers, or \code{\link{NULL}}.
#'
#' @inherit filter_lifecycle params references seealso return
#'
#' @seealso \code{\link[bupaR]{lifecycle_id}}
#'
#' @family filters
#'
#' @concept filters_event
#'
#' @export filter_trim_lifecycle
filter_trim_lifecycle <- function(log,
								  start_lifecycles = NULL,
								  end_lifecycles = NULL,
								  reverse = FALSE) {
	UseMethod("filter_trim_lifecycle")
}

#' @describeIn filter_trim_lifecycle Filters activity instances for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_trim_lifecycle.eventlog <- function(log,
										   start_lifecycles = NULL,
										   end_lifecycles = NULL,
										   reverse = FALSE) {

	UNIQUE_EVENT_ID <- NULL

	if(is.null(start_lifecycles) & is.null(end_lifecycles))
		stop("At least one start or end life cycle should be provided")

	acts <- (log) %>% pull(!!lifecycle_id_(log)) %>% unique()

	min_order <- NULL
	min_timestamp <- NULL
	start_r <- NULL
	end_r <- NULL
	min_rank <- NULL
	max_rank <- NULL
	r <- NULL
	if(is.null(start_lifecycles))
		start_lifecycles <- acts
	if(is.null(end_lifecycles))
		end_lifecycles <- acts

	log %>%
		mutate(UNIQUE_EVENT_ID = 1:n()) -> log

	log %>%
		filter_lifecycle_presence(start_lifecycles, method = "one_of") %>%
		filter_lifecycle_presence(end_lifecycles, method = "one_of") %>%
		group_by(!!activity_instance_id_(log)) %>%
		arrange(!!timestamp_(log), .order) %>%
		mutate(r = 1:n()) %>%
		mutate(start_r = ifelse((!!lifecycle_id_(log)) %in% start_lifecycles, r, NA),
			   end_r = ifelse((!!lifecycle_id_(log)) %in% end_lifecycles, r, NA)) %>%
		mutate(min_rank = min(c(Inf,start_r), na.rm = TRUE)) %>%
		mutate(max_rank = max(c(-Inf,end_r), na.rm = TRUE)) %>%
		filter( r >= min_rank, r <= max_rank) %>%
		pull(UNIQUE_EVENT_ID) -> selection

	if(!reverse)
		filter(log, UNIQUE_EVENT_ID %in% selection) %>% select(-UNIQUE_EVENT_ID)
	else
		filter(log, UNIQUE_EVENT_ID %in% selection) %>% select(-UNIQUE_EVENT_ID)
}

#' @describeIn filter_trim_lifecycle Filters activity instances for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
filter_trim_lifecycle.grouped_eventlog <- function(log,
												   start_lifecycles = NULL,
												   end_lifecycles = NULL,
												   reverse = FALSE) {

	apply_grouped_fun(log, fun = filter_trim_lifecycle.eventlog, start_lifecycles, end_lifecycles, reverse, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_trim_lifecycle Filter interactively
#' @export ifilter_trim_lifecycle
ifilter_trim_lifecycle <- function(log) {
	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Trim cases"),
		miniContentPanel(
			fillCol(flex = c(5,3,2),
					fillRow(flex = c(10,1,10),
							selectizeInput("start", label = "Select start life cycle labels:",
										   choices = log %>% pull(!!lifecycle_id_(log)) %>%
										   	unique %>% sort, selected = NA,  multiple = T), " ",
							selectizeInput("end", label = "Select end life cycle labels:",
										   choices = log %>% pull(!!lifecycle_id_(log)) %>%
										   	unique %>% sort, selected = NA,  multiple = T)),
					fillRow(
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")),
					"Trim all activity instances from the first event of a set of start life cycle labels to the last event of a set of end life cycle labels. Activity instances that do not have at least one event of both sets are discarded."			)

		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list(start_lifecycle = list(input$start),
													   end_lifecycle = list(input$end),
													   reverse = list(input$reverse == "Yes", FALSE)))


			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Trim cases", height = 400))

}


