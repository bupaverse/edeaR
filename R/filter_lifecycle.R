#' Filter: Life cycle
#'
#' Filters the log based on the life cycle id
#'
#' The method filter_lifecycle can be used to filter on life cycle identifiers. It has an lifecycle argument,
#' to which a vector of identifiers can be given. The selection can be negated with the reverse argument.
#'
#' @param eventlog The dataset to be used. Should be a (grouped) eventlog object.
#'
#' @param lifecycle Character vector containing one or more life cycle identifiers.
#' @param reverse Logical, indicating whether the selection should be reversed.
#' @param ... Deprecated arguments.
#'
#' @seealso \code{vignette("filters", "edeaR")}
#'
#' @return When given an eventlog, it will return a filtered eventlog. When given a grouped eventlog, the filter will be applied
#' in a stratified way (i.e. each separately for each group). The returned eventlog will be grouped on the same variables as
#' the original event log.
#'
#' @export
filter_lifecycle <- function(eventlog, lifecycle, reverse, ...) {
	UseMethod("filter_lifecycle")
}

#' @describeIn filter_lifecycle Filter eventlog on life cycle labels
#' @export

filter_lifecycle.eventlog <- function(eventlog,
									  lifecycle,
									 reverse = FALSE,
									 ...){
	if(reverse == FALSE)
		filter(eventlog, (!!lifecycle_id_(eventlog)) %in% lifecycle)
	else
		filter(eventlog, !((!!lifecycle_id_(eventlog)) %in% lifecycle))
}

#' @describeIn filter_lifecycle Filter grouped eventlog on life cycle labels
#' @export

filter_lifecycle.grouped_eventlog <- function(eventlog,
											  lifecycle,
											 reverse = FALSE,
											 ...){

	grouped_filter(eventlog, filter_lifecycle, lifecycle, reverse)
}


#' @rdname filter_lifecycle
#' @export ifilter_lifecycle
ifilter_lifecycle <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter life cycle"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_activities", label = "Select life cycle:", choices = eventlog %>% pull(!!lifecycle_id_(eventlog)) %>%
								   	unique %>% sort, selected = NA,  multiple = TRUE), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)
	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_lifecycle(eventlog,
											 lifecycle = input$selected_activities,
											reverse = ifelse(input$reverse == "Yes", TRUE, FALSE))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Life Cycle", height = 400))

}
