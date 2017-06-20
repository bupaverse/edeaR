#' @title Filter: Filter based on percentile of start and end activities
#'
#' @description Filters the log based on a provided set of start and end activities
#'
#' @param eventlog The event log to be used. An object of class
#' \code{eventlog}.
#'
#'
#' @param start_activities Start activities used for filtering.
#'
#' @param end_activities End activities used for filtering.
#'
#' @param percentile_cut_off Alternatively to using (sets of) start or end activities, a percentile cut off can be provided.
#' A percentile cut off value of 0.9 will return the cases starting and ending with the  90\% most common start and end activities.
#' When \code{reverse} is set to TRUE, it will return the 10\% cases with the least common start and end activivities.
#'
#' @param reverse A logical parameter depicting whether the selection should be reversed.
#'
#'
#' @export filter_endpoints

filter_endpoints <- function(eventlog,
							 start_activities = NULL,
							 end_activities = NULL,
							 percentile_cut_off = NULL,
							 reverse = F) {

	stop_eventlog(eventlog)

	if(is.null(start_activities) & is.null(end_activities) & is.null(percentile_cut_off))
		stop("At least one set of start or end activities or a percentile cut off must be provided.")


	if((!is.null(start_activities) & !is.null(percentile_cut_off)) | (!is.null(end_activities) & !is.null(percentile_cut_off)))
		stop("Cannot filter on both sets of start and end activities and percentile cut off simultaneously.")

	if(!is.null(percentile_cut_off))
		return(filter_endpoints_percentile(eventlog,
										   percentile_cut_off = percentile_cut_off,
										   reverse = reverse ))
	else
		return(filter_endpoints_sets(eventlog,
									 start_activities = start_activities,
									 end_activities = end_activities,
									 reverse = reverse))
}
#' @rdname filter_endpoints
#' @export ifilter_endpoints
#'

ifilter_endpoints <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter End Points"),
		miniContentPanel(
			fillCol(flex = c(3,8),
					fillRow(
						radioButtons("filter_type", "Filter type:", choices = c("Select from list" = "list", "Use percentile cutoff" = "percentile")),
						radioButtons("reverse", "Reverse filter: ", choices = c("Yes","No"), selected = "No")
					),
					uiOutput("filter_ui")
					)
		)
	)

	server <- function(input, output, session){

		output$filter_ui <- renderUI({
			if(input$filter_type == "list") {
				fillRow(flex = c(8,1,8),
					selectizeInput("start", label = "Select start activities:",
								   choices = eventlog %>%
								   	start_activities_activity %>%
								   	pull(!!as.symbol(activity_id(eventlog))) %>%
								   	unique, selected = NA,  multiple = T), " ",
					selectizeInput("end", label = "Select end activities:",
								   choices = eventlog %>%
								   	end_activities_activity %>%
								   	pull(!!as.symbol(activity_id(eventlog))) %>%
								   	unique, selected = NA,  multiple = T)
				) %>% return()
			}
			else if(input$filter_type == "percentile") {
				sliderInput("percentile_slider", "Percentile cut off:", min = 0, max = 100, value = 80) %>% return()
			}
		})



		observeEvent(input$done, {
			if(input$filter_type == "list")
				filtered_log <- filter_endpoints(eventlog,
												 start_activities = input$start,
												 end_activities = input$end,
												 reverse = ifelse(input$reverse == "Yes", T, F))
			else if(input$filter_type == "percentile") {
				filtered_log <- filter_endpoints(eventlog,
												percentile_cut_off = input$percentile_slider/100,
												 reverse = ifelse(input$reverse == "Yes", T, F))
			}

			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter End Points", height = 400))

}


