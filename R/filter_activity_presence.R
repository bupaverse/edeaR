#' Filter: Activity Presence
#'
#' Filters cases based on the presence (or absence) of activities
#'
#' This functions allows to filter cases that contain certain activities.
#' It requires as input a vector containing one or more activity labels and it has a method
#' argument. The latter can have the values all, none or one_of.
#' \itemize{
#' \item When set to `all`, it means
#' that all the specified activity labels must be present for a case to be selected
#' \item `none` means
#' that they are not allowed to be present.
#' \item `one_of` means that at least one of them must be
#' present.
#' \item `only` means that only (a set of) these activities are allowed to be present
#' \item `exact` means that only exactly these activities can be present (although multiple times and in random orderings)
#' }
#'
#' When only one activity label is supplied, note that methods all and one_of will be identical.
#'
#'
#' @inherit filter_activity params references seealso return
#'
#' @param method Filter method. If "all", each of the activities should be present. If "one_of", at least one of them should be present. If "none", none of the activities are allowed to occur in the filtered traces.
#'
#' @export filter_activity_presence
filter_activity_presence <- function(eventlog, activities, method, reverse) {
	UseMethod("filter_activity_presence")
}


#' @describeIn filter_activity_presence Filter event log on presence of activities.
#' @export

filter_activity_presence.eventlog <- function(eventlog,
											  activities = NULL,
											  method = c("all", "one_of", "none", "exact","only"),
											  reverse = FALSE){

	in_selection <- NULL
	in_ <- NULL
	out_ <- NULL

	method <- match.arg(method)


	eventlog %>%
		select(!!as.symbol(activity_id(eventlog)), !!as.symbol(case_id(eventlog)), force_df = T) %>%
		unique() %>%
		mutate(in_selection = !!activity_id_(eventlog) %in% activities) %>%
		group_by(!!as.symbol(case_id(eventlog)), in_selection) %>%
		summarize(n = n_distinct(!!activity_id_(eventlog))) %>%
		mutate(in_selection = ifelse(in_selection, "in_", "out_")) %>%
		spread(in_selection, n, fill = 0) -> selection

	if(method == "all")
		filter_case(eventlog, selection %>% filter(in_ == length(activities)) %>% pull(1), reverse)
	else if(method == "one_of")
		filter_case(eventlog, selection %>% filter(in_ >= 1) %>% pull(1), reverse)
	else if (method == "none")
		filter_case(eventlog, selection %>% filter(in_ == 0) %>% pull(1), reverse)
	else if (method == "exact")
		filter_case(eventlog, selection %>% filter(in_ == length(activities), out_ == 0) %>% pull(1), reverse)
	else if (method == "only")
		filter_case(eventlog, selection %>% filter(out_ == 0) %>% pull(1), reverse)




	# if(method == "all") {
	# 	eventlog %>%
	# 		group_by_case %>%
	# 		filter(all(activities %in% !!activity_id_(eventlog))) %>%
	# 		ungroup_eventlog
	#
	# } else if (method == "one_of") {
	# 	eventlog %>%
	# 		group_by_case %>%
	# 		filter(any(activities %in% !!activity_id_(eventlog))) %>%
	# 		ungroup_eventlog()
	# } else if (method  == "none") {
	# 	eventlog %>%
	# 		group_by_case %>%
	# 		filter(!any(activities %in% !!activity_id_(eventlog))) %>%
	# 		ungroup_eventlog
	# 	}

}

#' @describeIn filter_activity_presence Filter grouped event log on presence of activities.
#' @export
filter_activity_presence.grouped_eventlog <- function(eventlog,
											  activities = NULL,
											  method = c("all", "one_of", "none", "exact","only"),
											  reverse = FALSE) {
	grouped_filter(eventlog, filter_activity_presence, activities, method)
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
										   	unique, selected = NA,  multiple = TRUE), " ",
							radioButtons("method", "Method: ", choices = c("All" = "all","One of"= "one_of","None" = "none", "Exact" = "exact","Only" = "only"), selected = "all")
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
