#' @title Filter directly follows with time interval
#'
#' @description Filter cases where the activity `from` is followed by activity `to` within a certain time `interval`.
#'
#' @param interval [`numeric`] vector of length 2: A duration interval. Half open interval can be created using [`NA`].
#' @param from,to [`character`] vector of length 1: The antecendent and consequent to filter on. Both are [`character`]
#' vectors containing exactly one activity identifier.
#' @param units [`character`] (default `"secs"`): The time unit in which the processing times should be reported. Should be one of the following values:
#' `"secs"` (default), `"mins"`, `"hours"`, `"days"`, `"weeks"`. See also the `units` argument of [`difftime()`].
#'
#' @inherit filter_activity params references seealso return
#'
#' @seealso [`processing_time()`],[`difftime()`]
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_flow_time
filter_flow_time <- function(log,
							 from,
							 to,
							 interval,
							 reverse = FALSE,
							 units = c("secs", "mins", "hours", "days", "weeks")) {
	UseMethod("filter_flow_time")
}

#' @describeIn filter_flow_time Filters on flow time for a [`bupaR::log`].
#' @export
filter_flow_time.log <- function(log,
								 from,
								 to,
								 interval,
								 reverse = FALSE,
								 units = c("secs", "mins", "hours", "days", "weeks")) {

	units <- rlang::arg_match(units)

	if(!is.null(interval) && (length(interval) != 2 || !is.numeric(interval) || any(interval < 0, na.rm = T) || all(is.na(interval)) )) {
		cli_abort(c("{.arg interval} should be a positive {.cls numeric} vector of length 2.",
					"x" = "You supplied a {.cls {class(interval)}}: {.val {interval}}",
					"i" = "One of the elements can be {.code NA} to create open intervals."))
	}

	if(is.null(interval))
		cli_abort(c("Invalid {.arg interval}",
					"x" = "{.arg interval} cannot be {.code NULL}"))
	else { #if(!is.null(interval))
		lower_threshold <- ifelse(is.na(interval[1]), -Inf, interval[1])
		upper_threshold <- ifelse(is.na(interval[2]), Inf, interval[2])

		create_precedence_df(log) %>%
			mutate(across(c("next_activity","AID"), as.character)) %>%
			filter(.data[["AID"]] == from & .data[["next_activity"]] == to) %>%
			mutate(idle_time = as.double(.data[["next_start_time"]] - .data[["end_time"]], units = units)) %>%
			# filter for idle time between activities in the interval
			filter(between(idle_time, lower_threshold, upper_threshold)) %>%
			pull(.data[["CID"]]) %>%
			unique() -> case_selection

		filter_case(log = log, cases = case_selection, reverse)
	}
}

#' @describeIn filter_flow_time Filters on flow time for a [`bupaR::grouped_log`].
#' @export
filter_flow_time.grouped_log <- function(log,
										 from,
										 to,
										 interval,
										 reverse = FALSE,
										 units = c("secs", "mins", "hours", "days", "weeks")) {


	apply_grouped_fun(log, fun = filter_flow_time.log, interval, reverse, units, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_flow_time Filter interactively
#' @export ifilter_flow_time
ifilter_flow_time <- function(log) {

	next_start_time <-NULL
	end_time <- NULL
	AID <- NULL
	next_activity <- NULL
	flow <- NULL
	min_flow <- NULL
	max_flow <- NULL

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter flow time"),
		miniContentPanel(
			fillCol(flex = c(1,1,2),
					fillRow(flex = c(10,1,10),
							selectizeInput("ante", label = "From:",
										   choices = log %>% pull(!!as.symbol(activity_id(log))) %>%
										   	unique, selected = NA,  multiple = FALSE), " ",
							selectizeInput("conse", label = "To:",
										   choices = log %>% pull(!!as.symbol(activity_id(log))) %>%
										   	unique, selected = NA,  multiple = FALSE)),
					fillRow(
						uiOutput("filter_ui")
					),
					fillRow(
						radioButtons("units", "Time units: ", choices = c("secs", "mins", "hours", "days", "weeks"), selected = "secs"),
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No"))

			)
		)
	)

	server <- function(input, output, session){
		flow_time <- reactive({
			log %>%
				create_precedence_df() %>%
				mutate(flow = as.double(next_start_time - end_time, units = input$units)) %>%
				group_by(AID, next_activity) %>%
				summarize(min_flow = min(flow), max_flow = max(flow)) %>%
				ungroup()
		})


		output$filter_ui <- renderUI({
			sliderInput("interval_slider", "Process time interval",
						min = ifelse(nrow(filter(flow_time(), AID == input$ante, next_activity == input$conse)) > 0, filter(flow_time(), AID == input$ante, next_activity == input$conse) %>% pull(min_flow) %>% round(2)-1,0),
						max = ifelse(nrow(filter(flow_time(), AID == input$ante, next_activity == input$conse)) > 0, filter(flow_time(), AID == input$ante, next_activity == input$conse) %>% pull(max_flow) %>% round(2)+1,0),
						value = c(ifelse(nrow(filter(flow_time(), AID == input$ante, next_activity == input$conse)) > 0, filter(flow_time(), AID == input$ante, next_activity == input$conse) %>% pull(min_flow) %>% round(2),0),
								  ifelse(nrow(filter(flow_time(), AID == input$ante, next_activity == input$conse)) > 0, filter(flow_time(), AID == input$ante, next_activity == input$conse) %>% pull(max_flow) %>% round(2),0)))
		})

		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list(from = list(input$ante),
													   to = list(input$conse),
													   interval = list(input$interval_slider),
													   units = list(input$units, "'secs'"),
													   reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter flow time", height = 400))

}

