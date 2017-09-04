#' @title Filter: Resource frequency
#'
#' @description Filters the log based on its most frequent resources, until a specific percentile cut off.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param percentile_cut_off The target coverage of events
#' A percentile of 0.9 will return the most common resource types of the eventlog, which account for 90\% of the events.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_resource_frequency
#'
filter_resource_frequency <- function(eventlog,
									  percentile_cut_off = 0.8,
									  reverse = F) {
	stop_eventlog(eventlog)
	mapping <- mapping(eventlog)

	act_freq <- resources(eventlog) %>%
		arrange(-absolute_frequency) %>%
		mutate(r = cumsum(relative_frequency))

	if(reverse == F)
		event_selection <- act_freq %>% filter(r <= percentile_cut_off)

	else
		event_selection <- act_freq %>% filter(r > percentile_cut_off)


	colnames(event_selection)[colnames(event_selection) == resource_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == resource_id(eventlog)] <- "event_classifier"

	event_selection <- select(event_selection, event_classifier)

	output <- filter(eventlog, event_classifier %in% event_selection$event_classifier)

	colnames(output)[colnames(output)=="event_classifier"] <- resource_id(eventlog)

	output <- output %>%
		re_map(mapping)

	return(output)
}


#' @rdname filter_resource_frequency
#' @export ifilter_resource_frequency
ifilter_resource_frequency <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter resources based on frequency"),
		miniContentPanel(
			fillCol(flex = c(2,1),
					fillRow(flex = c(10,1,8),
							sliderInput("percentile_cut_off", "Cumulative Percentile Cut-off", 0, 100, value = 80),
							" ",
							radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
					),
					"A percentile of 0.9 will return the most common resources of the eventlog, which account for 90% of the events."
			))
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_resource_frequency(eventlog,
													  percentile_cut_off = input$percentile_cut_off/100,
													  reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter resources based on frequency", height = 400))

}
