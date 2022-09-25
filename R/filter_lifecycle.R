#' @title Filter Life Cycle
#'
#' @description Filters the log based on the life cycle identifier.
#'
#' @param lifecycles \code{\link{character}} vector: A vector of life cycle identifiers.
#' @param lifecycle `r lifecycle::badge("deprecated")`; please use \code{lifecycles} instead.
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
							 reverse = FALSE,
							 lifecycle = deprecated(),
							 eventlog = deprecated()) {
	UseMethod("filter_lifecycle")
}

#' @describeIn filter_lifecycle Filters based on life cycle identifiers for an \code{\link[bupaR]{eventlog}}.
#' @export
filter_lifecycle.eventlog <- function(log,
									  lifecycles,
									  reverse = FALSE,
									  lifecycle = deprecated(),
									  eventlog = deprecated()) {

	if(lifecycle::is_present(eventlog)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_lifecycle(eventlog)",
			with = "filter_lifecycle(log)")
		log <- eventlog
	}
	if(lifecycle::is_present(lifecycle)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_lifecycle(lifecycle)",
			with = "filter_lifecycle(lifecycles)")
		lifecycles <- lifecycle
	}

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
											  reverse = FALSE,
											  lifecycle = deprecated(),
											  eventlog = deprecated()){

	log <- lifecycle_warning_eventlog(log, eventlog)
	if(lifecycle::is_present(lifecycle)) {
		lifecycle::deprecate_warn(
			when = "0.9.0",
			what = "filter_lifecycle(lifecycle)",
			with = "filter_lifecycle(lifecycles)")
		lifecycles <- lifecycle
	}

	bupaR:::apply_grouped_fun(log, fun = filter_lifecycle.eventlog, lifecycles, reverse, .ignore_groups = TRUE, .keep_groups = TRUE, .returns_log = TRUE)
}

#' @keywords internal
#' @rdname ifilter
#' @export ifilter_lifecycle
ifilter_lifecycle <- function(eventlog) {

	lifecycle::deprecate_warn("0.9.0", "ifilter_lifecycle()")

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
