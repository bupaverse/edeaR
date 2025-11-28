#' @title Filter Infrequent Flows
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Filter cases based on infrequent flows.
#'
#' @param min_n [`numeric`] (default `2`): Cases containing a flow that occurs less than `min_n` times are discarded.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export
filter_infrequent_flows <- function(log, min_n = 2) {
	UseMethod("filter_infrequent_flows")
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for an [`eventlog`][`bupaR::eventlog`].
#' @export
filter_infrequent_flows.eventlog <- function(log, min_n = 2) {

	next_act <- NULL

	if(min_n <= 1) {
		stop("min_n should be at least 2")
	}

	log %>%
		add_start_activity("START_ACT") %>%
		create_minimal_activity_log() %>%
		select(case_id(log), activity_id(log), timestamp(log), .order) %>%
		mutate(across(.cols = activity_id(log), as.character)) %>%
		group_by(!!case_id_(log)) %>%
		arrange(!!timestamp_(log)) %>%
		mutate(next_act = lead(!!activity_id_(log), default = "END_ACT")) %>%
		select(!!case_id_(log), !!activity_id_(log), next_act) %>%
		nest(data = c(!!case_id_(log))) %>%
		mutate(n = map_int(data, nrow)) -> flow_info

	# log %>%
	# 	add_start_activity("START_ACT") %>%
	# 	data.table() -> dt
	#
	# # For each case, keep only events with minimum timestamp and .order
	# cols <- c(case_id(log), activity_id(log), timestamp(log), ".order")
	# setorderv(dt, cols = c(case_id(log), timestamp(log), ".order"))
	# dt <- unique(dt, by = c(case_id(log), activity_instance_id(log), activity_id(log)))[,
	#              ..cols]
	#
	# # Order each case by timestamp
	# setorderv(dt, cols = c(case_id(log), timestamp(log)))


	while(min(flow_info$n) < min_n & nrow(flow_info) > 0) {
		flow_info %>%
			filter(n < min_n) %>%
			unnest(data) %>%
			pull(!!case_id_(log)) %>%
			unique() -> remove

		flow_info %>%
			filter(n >= min_n) %>%
			mutate(data = map(data, ~filter(.x, !(!!case_id_(log) %in% remove)))) %>%
			mutate(n = map_int(data, nrow)) -> flow_info

	}

	flow_info %>%
		unnest(data) %>%
		pull(!!case_id_(log)) %>%
		unique() -> keep

	log <- filter_case.log(log, cases = keep, reverse = FALSE)

	return(log)
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for a [`grouped_eventlog`][`bupaR::grouped_eventlog`].
#' @export
filter_infrequent_flows.grouped_eventlog <- function(log, min_n = 2) {

	apply_grouped_fun(log, filter_infrequent_flows.eventlog, min_n, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for an [`activitylog`][`bupaR::activitylog`].
#' @export
filter_infrequent_flows.activitylog <- function(log, min_n = 2) {

	log %>%
		to_eventlog() %>%
		mutate(!!activity_instance_id(.) := as.character(.data[[activity_instance_id(.)]])) %>%
		filter_infrequent_flows.eventlog(min_n = min_n) %>%
		to_activitylog()
}

#' @describeIn filter_infrequent_flows Filters infrequent flows for a [`grouped_activitylog`][`bupaR::grouped_activitylog`].
#' @export
filter_infrequent_flows.grouped_activitylog <- function(log, min_n = 2) {

	apply_grouped_fun(log, filter_infrequent_flows.activitylog, min_n, .ignore_groups = FALSE, .keep_groups = FALSE, .returns_log = FALSE)
}


#' @describeIn filter_infrequent_flows Filter interactively
#' @export ifilter_infrequent_flows
ifilter_infrequent_flows <- function(log) {

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter infrequent flows"),
		miniContentPanel(
			fillCol(flex = c(2,1,5),
					fillRow(
						uiOutput("filter_ui")
					),
					h5("Current infrequent flows"),
					tableOutput("infrequent_flows")

			)
		)
	)

	server <- function(input, output, session){

		AID <- NULL
		next_activity <- NULL

		max_flow_n <- suppressWarnings({log %>%
				create_precedence_df() %>%
				count(AID, next_activity) %>%
				filter(AID != "Start", next_activity != "End") %>%
				pull(n) %>%
				max() })

		flows <- reactive({
			suppressWarnings({
				log %>%
					filter_infrequent_flows(min_n = ifelse(length(input$interval_slider) == 0, 2, input$interval_slider)) %>%
					create_precedence_df() %>%
					count(AID, next_activity) %>%
					filter(AID != "Start", next_activity != "End")

			})
		})

		output$filter_ui <- renderUI({
			numericInput("interval_slider", "Min frequency",
						 min = 2,
						 value = 2)
		})
		output$infrequent_flows <- renderTable({
			flows() %>%
				arrange(n) %>%
				rename(from = "AID",
					   to = "next_activity") %>%
				head(10)
		})

		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list(min_n = list(input$interval_slider)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter infrequent flows", height = 400))

}
