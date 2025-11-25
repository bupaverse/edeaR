#' @title Filter Life Cycle
#'
#' @description Filters the log based on the life cycle identifier.
#'
#' @param lifecycles \code{\link{character}} vector: A vector of life cycle identifiers.
#'
#' @inherit filter_activity_instance params references seealso return
#'
#' @seealso \code{\link[bupaR]{lifecycle_id}}
#'
#' @family filters
#'
#' @concept filters_event
#'
#' @export
filter_lifecycle <- function(log,
							 lifecycles,
							 reverse = FALSE) {
	UseMethod("filter_lifecycle")
}

#' @describeIn filter_lifecycle Filters based on life cycle identifiers for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_lifecycle.eventlog <- function(log,
									  lifecycles,
									  reverse = FALSE) {
	if(!reverse) {
		log %>%
			filter(.data[[lifecycle_id(.)]] %in% lifecycles)
	} else {
		log %>%
			filter(!(.data[[lifecycle_id(.)]] %in% lifecycles))
	}
}

#' @describeIn filter_lifecycle Filters based on life cycle identifiers a \code{\link[bupaR]{grouped_eventlog}}.
#' @export
filter_lifecycle.grouped_eventlog <- function(log,
											  lifecycles,
											  reverse = FALSE){

	apply_grouped_fun(log, fun = filter_lifecycle.eventlog, lifecycles, reverse, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_lifecycle Filter interactively
#' @export ifilter_lifecycle
ifilter_lifecycle <- function(log) {

	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter life cycle"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_lifecycles", label = "Select life cycle:", choices = log %>% pull(!!lifecycle_id_(log)) %>%
								   	unique %>% sort, selected = NA,  multiple = TRUE), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {

			fun_call <- construct_call(input_cmd, list(lifecycles = list(input$selected_lifecycles), reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Life Cycle", height = 400))

}
