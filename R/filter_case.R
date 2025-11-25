#' @title Filter Case
#'
#' @description Filters the log based on case identifier. This method has a \code{cases} argument,
#' to which a vector of identifiers can be given. The selection can be negated with the \code{reverse} argument.
#'
#' @param cases \code{\link{character}} vector: A vector of cases identifiers.
#'
#' @inherit filter_activity params references seealso return
#'
#' @family filters
#'
#' @concept filters_case
#'
#' @export filter_case
filter_case <- function(log, cases, reverse = FALSE) {
	UseMethod("filter_case")
}

#' @describeIn filter_case Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_case.log <- function(log, cases, reverse = FALSE) {

	if(length(cases) == 0) {
		if(!reverse) {
			#return empty log
			log %>%
				filter(FALSE)
		} else {
			log
		}

	} else if(!reverse) {
		log %>%
			filter(.data[[case_id(.)]] %in% cases)
	} else {
		log %>%
			filter(!(.data[[case_id(.)]] %in% cases))
	}
}

#' @describeIn filter_case Filters cases for a \code{\link[bupaR]{grouped_log}}.
#' @export
filter_case.grouped_log <- function(log, cases, reverse = FALSE) {

	apply_grouped_fun(log, fun = filter_case.log, cases, reverse, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @describeIn filter_case Filter interactively
#' @export ifilter_case
ifilter_case <- function(log) {
	input_cmd <- construct_input_call(sys.calls(), deparse(substitute(log)))

	ui <- miniPage(
		gadgetTitleBar("Filter Cases"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_cases", label = "Select cases:", choices = log %>% pull(!!as.symbol(case_id(log))) %>%
								   	unique, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {
			fun_call <- construct_call(input_cmd, list(cases = list(input$selected_cases),reverse = list(input$reverse == "Yes", FALSE)))

			result <- eval(parse_expr(fun_call))
			rstudioapi::sendToConsole(fun_call)
			stopApp(result)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Cases", height = 400))

}
