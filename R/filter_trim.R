#' @title Trim Cases
#'
#' @description Trim cases from the first event of a set of start activities to the last event of a set of end activities.
#'
#' One can trim cases by removing one or more activity instances at the start and/or end of a case. Trimming is performed
#' until all cases have a start and/or end point belonging to a set of allowed activity labels. This filter requires a set
#' of allowed start activities and/or a set of allowed end activities. If one of them is not provided it will not trim the
#' cases at this edge. The selection can be reversed, which means that only the trimmed events at the start and end of cases
#' are retained. As such, this argument allows to cut intermediate parts out of traces.
#'
#' @inherit filter_activity params references seealso return
#' @inherit filter_endpoints params
#'
#' @family filters
#'
#' @concept filters_event
#'
#' @export filter_trim
filter_trim <- function(log,
						start_activities = NULL,
						end_activities = NULL,
						reverse = FALSE) {
	UseMethod("filter_trim")
}

#' @describeIn filter_trim Filters activity instances for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_trim.eventlog <- function(log,
								 start_activities = NULL,
								 end_activities = NULL,
								 reverse = FALSE) {

	if(is.null(start_activities) & is.null(end_activities))
		stop("At least one start or end activity should be provided")

	acts <- activities(log) %>% pull(1)

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

	log %>%
		filter(.data[[activity_id(log)]] %in% c(start_activities)) -> has_start
	log %>%
		filter(.data[[activity_id(log)]] %in% c(end_activities)) -> has_end

	log %>%
		filter(.data[[case_id(log)]] %in% has_start[[case_id(log)]], .data[[case_id(log)]] %in% has_end[[case_id(log)]]) -> candidate_cases

	if(nrow(candidate_cases) == 0) {
		log %>%
			filter(FALSE)
	} else {
		log %>%
			group_by(!!case_id_(log), !!activity_instance_id_(log), !!activity_id_(log)) %>%
			summarize(min_timestamp = min(!!timestamp_(log), Inf), "min_order" = min(.order, Inf), n = n()) %>%
			as.data.frame() %>%
			filter(n > 0) %>%
			group_by(!!case_id_(log)) %>%
			arrange(min_timestamp, min_order) %>%
			mutate(r = 1:n()) %>%
			mutate(start_r = ifelse((!!activity_id_(log)) %in% start_activities, r, NA),
				   end_r = ifelse((!!activity_id_(log)) %in% end_activities, r, NA)) %>%
			mutate(min_rank = min(c(Inf,start_r), na.rm = TRUE)) %>%
			mutate(max_rank = max(c(-Inf,end_r), na.rm = TRUE)) %>%
			filter( r >= min_rank, r <= max_rank) %>%
			pull(!!activity_instance_id_(log)) -> aid_selection

		filter_activity_instance.eventlog(log, activity_instances = aid_selection, reverse = reverse)
	}
}

#' @describeIn filter_trim Filters activity instances for a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
filter_trim.grouped_eventlog <- function(log,
										 start_activities = NULL,
										 end_activities = NULL,
										 reverse = FALSE) {

	bupaR:::apply_grouped_fun(log, fun = filter_trim.eventlog, start_activities, end_activities, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_trim Filters activity instances for an \code{\link[bupaR]{activitylog}}.
#' @export
filter_trim.activitylog <- function(log,
									start_activities = NULL,
									end_activities = NULL,
									reverse = FALSE) {

	filter_trim.eventlog(bupaR::to_eventlog(log),
						 start_activities = start_activities,
						 end_activities = end_activities,
						 reverse = reverse) %>%
		to_activitylog()
}

#' @describeIn filter_trim Filters activity instances for a \code{\link[bupaR]{grouped_activitylog}}.
#' @export
filter_trim.grouped_activitylog <- function(log,
											start_activities = NULL,
											end_activities = NULL,
											reverse = FALSE) {

	filter_trim.grouped_eventlog(bupaR::to_eventlog(log),
								 start_activities = start_activities,
								 end_activities = end_activities,
								 reverse = reverse) %>%
		to_activitylog()
}
#' @describeIn filter_trim Filter interactively
#' @export ifilter_trim
ifilter_trim <- function(log) {

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Trim cases"),
		miniContentPanel(
			fillCol(flex = c(5,3,2),
					fillRow(flex = c(10,1,10),
							selectizeInput("start", label = "Select start activities:",
										   choices = log %>% pull(!!activity_id_(log)) %>%
										   	unique %>% sort, selected = NA,  multiple = T), " ",
							selectizeInput("end", label = "Select end activities:",
										   choices = log %>% pull(!!activity_id_(log)) %>%
										   	unique %>% sort, selected = NA,  multiple = T)),
					fillRow(
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")),
					"Trim all cases from the first event of a set of start activities to the last event of a set of end activities. Traces that do not have at least one event of both sets are discarded."			)

		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list(start_activities = list(input$start),
													   end_activities = list(input$end), reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Trim cases", height = 400))

}


