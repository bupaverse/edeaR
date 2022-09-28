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
filter_case <- function(log, cases, reverse = FALSE, eventlog = deprecated()) {
	UseMethod("filter_case")
}

#' @describeIn filter_case Filters cases for a \code{\link[bupaR]{log}}.
#' @export
filter_case.log <- function(log, cases, reverse = FALSE, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_case(eventlog)",
			with = "filter_case(log)")
		log <- eventlog
	}

	if(length(cases) == 0) {
		#return empty log
		log %>%
			filter(FALSE)
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
filter_case.grouped_log <- function(log, cases, reverse = FALSE, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_case(eventlog)",
			with = "filter_case(log)")
		log <- eventlog
	}

	bupaR:::apply_grouped_fun(log, fun = filter_case.log, cases, reverse, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @keywords internal
#' @rdname ifilter
#' @export ifilter_case
ifilter_case <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_case()")

	ui <- miniPage(
		gadgetTitleBar("Filter Cases"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_cases", label = "Select cases:", choices = eventlog %>% pull(!!as.symbol(case_id(eventlog))) %>%
								   	unique, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_case(eventlog, cases = input$selected_cases, reverse = ifelse(input$reverse == "Yes", T, F))

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Cases", height = 400))

}
