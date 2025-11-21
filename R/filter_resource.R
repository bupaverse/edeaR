#' @title Filter Resource
#'
#' @description Filters the log based on resource identifiers
#'
#' This method can be used to filter on resource identifiers. It has a \code{resources} argument,
#' to which a vector of identifiers can be given. The selection can be negated with the \code{reverse} argument.
#'
#' @param resources \code{\link{character}} vector: A vector of resources identifiers.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_event
#'
#' @export filter_resource
filter_resource <- function(log, resources, reverse = FALSE) {
	UseMethod("filter_resource")
}

#' @describeIn filter_resource Filters resources for a \code{\link[bupaR]{log}}.
#' @export
filter_resource.log <- function(log, resources,	reverse = FALSE) {

	if (!reverse) {
		log %>%
			filter(.data[[resource_id(.)]] %in% resources)
	} else {
		log %>%
			filter(!(.data[[resource_id(.)]] %in% resources))
	}
}

#' @describeIn filter_resource Filters resources for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_resource.grouped_log <- function(log, resources, reverse = FALSE) {

	bupaR:::apply_grouped_fun(log, fun = filter_resource.log, resources, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_resource, resources, reverse)
}

#' @describeIn filter_resource Filter interactively
#' @export ifilter_resource
ifilter_resource <- function(log) {
	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter Resources"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_resources", label = "Select resources:", choices = log %>% pull(!!as.symbol(resource_id(log))) %>%
								   	unique %>% sort, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {
			fun_call <- construct_call(input_cmd, list(resources = list(input$selected_resources),
													   reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Resources", height = 400))

}
