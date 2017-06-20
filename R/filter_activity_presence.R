#' @title Filter: Activity Presence
#'
#' @description Filters cases based on the presence (or absence) of activities
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param activities A vector of activities to withhold.
#'
#' @param method Filter method. If "all", each of the activities should be present. If "one_of", at least one of them should be present. If "none", none of the activities are allowed to occur in the filtered traces.
#'
#' @export filter_activity_presence
#'
filter_activity_presence <- function(eventlog,
							activities = NULL,
							method = c("all", "one_of", "none")){
	stop_eventlog(eventlog)

	e <- eventlog
	colnames(e)[colnames(e) == activity_id(e)] <- "event_classifier"
	colnames(e)[colnames(e) == case_id(e)] <- "case_classifier"

	method <- match.arg(method)

	e %>%
		filter(event_classifier %in% activities) %>%
		select(event_classifier, case_classifier) %>%
		unique() %>%
		group_by(case_classifier) %>%
		summarize(n = n()) -> selection

	selection %>%
		filter(n == length(activities)) -> selection_all

	if(method == "all")
		output <- filter_case(eventlog, selection_all$case_classifier)

	else if(method == "one_of")
		output <- filter_case(eventlog, selection$case_classifier)
	else if (method == "none")
		output <- filter_case(eventlog, selection$case_classifier, reverse = T)


	return(output)
}

#' @rdname filter_activity_presence
#' @export ifilter_activity_presence

ifilter_activity_presence <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter activities based on presence"),
		miniContentPanel(
			fillCol(flex = c(2,1),
					fillRow(flex = c(10,1,8),
							selectizeInput("selected_activities",
										   label = "Select activities:",
										   choices = eventlog %>% pull(!!as.symbol(activity_id(eventlog))) %>%
										   	unique, selected = NA,  multiple = T), " ",
							radioButtons("method", "Method: ", choices = c("All" = "all","One of"= "one_of","None" = "none"), selected = "all")
					),
					"If \"all\", each of the activities should be present.
					If \"one_of\", at least one of them should be present. If \"none\", none of the activities are allowed to occur in the filtered traces."
			))
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_activity_presence(eventlog,
													  activities = input$selected_activities,
													  method = input$method)


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter activities based on presence", height = 400))

}
