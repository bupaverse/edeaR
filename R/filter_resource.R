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
#' @export filter_resource
filter_resource <- function(log, resources, reverse = FALSE, eventlog = deprecated()) {
	UseMethod("filter_resource")
}

#' @describeIn filter_resource Filters resources for a \code{\link[bupaR]{log}}.
#' @export
filter_resource.log <- function(log, resources,	reverse = FALSE, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_resource(eventlog)",
			with = "filter_resource(log)")
		log <- eventlog
	}

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
filter_resource.grouped_log <- function(log, resources, reverse = FALSE, eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_resource(eventlog)",
			with = "filter_resource(log)")
		log <- eventlog
	}

	bupaR:::apply_grouped_fun(log, fun = filter_resource.log, resources, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
	#grouped_filter(eventlog, filter_resource, resources, reverse)
}

#' @rdname filter_resource
#' @keywords internal
#' @export ifilter_resource
ifilter_resource <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_resource()")

	ui <- miniPage(
		gadgetTitleBar("Filter Resources"),
		miniContentPanel(
			fillRow(flex = c(10,1,8),
					selectizeInput("selected_resources", label = "Select resources:", choices = eventlog %>% pull(!!as.symbol(resource_id(eventlog))) %>%
								   	unique %>% sort, selected = NA,  multiple = T), " ",
					radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			)
		)
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_resource(eventlog, resources = input$selected_resources, reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter Resources", height = 400))

}
