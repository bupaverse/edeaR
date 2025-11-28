#' @title Filter Trace
#'
#' @description Filters the log based on trace identifier.
#'
#' This method can be used to filter on trace identifier, which can be obtained from \code{\link[bupaR]{case_list}}.
#' It has a \code{trace_ids} argument, to which a vector of identifiers can be given. The selection can be negated with the \code{reverse} argument.
#'
#' @param trace_ids \code{\link{character}} vector: A vector of trace identifiers
#'
#' @inherit filter_activity params references seealso return
#'
#' @seealso \code{\link[bupaR]{case_list}}
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_trace
filter_trace <- function(log, trace_ids, reverse = FALSE) {
	UseMethod("filter_trace")
}

#' @describeIn filter_trace Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_trace.log <- function(log, trace_ids, reverse = FALSE){

	trace_id <- NULL

	log %>%
		case_list() %>%
		filter(trace_id %in% trace_ids) %>%
		pull(1) -> cases

	filter_case.log(log, cases = cases, reverse = reverse)
}

#' @describeIn filter_trace Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_trace.grouped_log <- function(log, trace_ids, reverse = FALSE) {

	apply_grouped_fun(log, fun = filter_trace.log, trace_ids, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_trace Filter interactively
#' @export ifilter_trace
ifilter_trace <- function(log) {
	trace_id <- NULL
	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	log %>%
		case_list() %>%
		count(trace, trace_id) -> tr

	trace_selection <- as.list(tr$trace_id)
	names(trace_selection) <- tr$trace

	ui <- miniPage(
		gadgetTitleBar("Filter Traces"),
		miniContentPanel(
			fillCol(flex = c(2,2),
				selectizeInput("selected_traces", label = "Select traces:", choices = trace_selection, selected = NA,  multiple = T, width = "100%"),
				radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {
			fun_call <- construct_call(input_cmd, list(trace_ids = list(input$selected_traces),reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Traces", height = 400))

}
