#' @title Filter: Activity frequency
#'
#' @description Filters the log based on its most frequent activities, until a specific percentile cut off.
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#' @param percentile_cut_off The target coverage of events
#' A percentile of 0.9 will return the most common activity types of the eventlog, which account for 90\% of the events.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#' @export filter_activity_frequency
#'
filter_activity_frequency <- function(eventlog,
									 percentile_cut_off = 0.8,
									 reverse = F) {
	stop_eventlog(eventlog)
	mapping <- mapping(eventlog)

	act_freq <- activities(eventlog) %>%
		arrange(-absolute_frequency) %>%
		mutate(r = cumsum(relative_frequency))

	if(reverse == F)
		event_selection <- act_freq %>% filter(r <= percentile_cut_off)

	else
		event_selection <- act_freq %>% filter(r > percentile_cut_off)


	colnames(event_selection)[colnames(event_selection) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"

	event_selection <- select(event_selection, event_classifier)

	output <- filter(eventlog, event_classifier %in% event_selection$event_classifier)

	colnames(output)[colnames(output)=="event_classifier"] <- activity_id(eventlog)

	output <- output %>%
		re_map(mapping)

	return(output)
}


#' @rdname filter_activity_frequency
#' @export ifilter_activity_frequency
ifilter_activity_frequency <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter activities based on frequency"),
		miniContentPanel(
			fillCol(flex = c(2,1),
				fillRow(flex = c(10,1,8),
			sliderInput("percentile_cut_off", "Cumulative Percentile Cut-off", 0, 100, value = 80),
			" ",
			radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
			),
			"A percentile of 0.9 will return the most common activity types of the eventlog, which account for 90% of the events."
		))
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_activity_frequency(eventlog,
													  percentile_cut_off = input$percentile_cut_off/100,
													  reverse = ifelse(input$reverse == "Yes", T, F))


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter activities based on frequency", height = 400))

}
