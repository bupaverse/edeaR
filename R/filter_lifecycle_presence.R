#' Filter: Life cycle Presence
#'
#' Filters activity instances based on the presence (or absence) of life cycles
#'
#' This functions allows to filter activity instances that (do not) contain certain life cycle identifiers.
#' It requires as input a vector containing one or more life cycle labels and it has a method
#' argument. The latter can have the values all, none or one_of.
#' \itemize{
#' \item When set to `all`, it means
#' that all the specified life cycle labels must be present for an activity instance to be selected
#' \item `none` means that they are not allowed to be present.
#' \item `one_of` means that at least one of them must be present.
#' \item `only` means that only (a set of) these life cycle labels are allowed to be present
#' \item `exact` means that only exactly these life cycle labels can be present (although multiple times and in random orderings)
#' }
#'
#'
#'
#' @inherit filter_activity params references seealso return
#'
#' @param method Filter method. If "all", each of the life cycle labels should be present. If "one_of", at least one of them should be present. If "none", none of the life cycle labels are allowed to occur in the filtered activity instances.
#' @param lifecycle Character vector containing one or more life cycle identifiers.

#' @export filter_lifecycle_presence
filter_lifecycle_presence <- function(eventlog, lifecycle, method, reverse) {
	UseMethod("filter_lifecycle_presence")
}


#' @describeIn filter_lifecycle_presence Filter event log on presence of life cycle labels.
#' @export

filter_lifecycle_presence.eventlog <- function(eventlog,
											   lifecycle = NULL,
											  method = c("all", "one_of", "none", "exact","only"),
											  reverse = FALSE){

	in_selection <- NULL
	in_ <- NULL
	out_ <- NULL

	method <- match.arg(method)

	eventlog %>%
		select(!!lifecycle_id_(eventlog), !!activity_instance_id_(eventlog), force_df = T) %>%
		unique() %>%
		mutate(in_selection = !!lifecycle_id_(eventlog) %in% lifecycle) %>%
		group_by(!!as.symbol(activity_instance_id_(eventlog)), in_selection) %>%
		summarize(n = n_distinct(!!lifecycle_id_(eventlog))) %>%
		mutate(in_selection = ifelse(in_selection, "in_", "out_")) %>%
		spread(in_selection, n, fill = 0) -> selection

	if(method == "all")
		filter_activity_instance(eventlog, selection %>% filter(in_ == length(lifecycle)) %>% pull(1), reverse)
	else if(method == "one_of")
		filter_activity_instance(eventlog, selection %>% filter(in_ >= 1) %>% pull(1), reverse)
	else if (method == "none")
		filter_activity_instance(eventlog, selection %>% filter(in_ == 0) %>% pull(1), reverse)
	else if (method == "exact")
		filter_activity_instance(eventlog, selection %>% filter(in_ == length(lifecycle), out_ == 0) %>% pull(1), reverse)
	else if (method == "only")
		filter_activity_instance(eventlog, selection %>% filter(out_ == 0) %>% pull(1), reverse)




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

#' @describeIn filter_lifecycle_presence Filter grouped event log on presence of life cycle labels.
#' @export
filter_lifecycle_presence.grouped_eventlog <- function(eventlog,
													   lifecycle = NULL,
													  method = c("all", "one_of", "none", "exact","only"),
													  reverse = FALSE) {
	grouped_filter(eventlog, filter_lifecycle_presence, activities, method)
}

#' @rdname filter_lifecycle_presence
#' @export ifilter_lifecycle_presence

ifilter_lifecycle_presence <- function(eventlog) {

	ui <- miniPage(
		gadgetTitleBar("Filter activities based on presence"),
		miniContentPanel(
			fillCol(flex = c(2,1),
					fillRow(flex = c(10,1,8),
							selectizeInput("selected_activities",
										   label = "Select life cycle:",
										   choices = eventlog %>% pull(!!as.symbol(lifecycle_id(eventlog))) %>%
										   	unique, selected = NA,  multiple = TRUE), " ",
							radioButtons("method", "Method: ", choices = c("All" = "all","One of"= "one_of","None" = "none", "Exact" = "exact","Only" = "only"), selected = "all")
					),
					"If \"all\", each of the life cycle labels should be present.
					If \"one_of\", at least one of them should be present. If \"none\", none of the life cycle labels are allowed to occur in the filtered traces."
			))
	)

	server <- function(input, output, session){
		observeEvent(input$done, {

			filtered_log <- filter_lifecycle_presence(eventlog,
													 lifecycle = input$selected_activities,
													 method = input$method)


			stopApp(filtered_log)
		})
	}
	runGadget(ui, server, viewer = dialogViewer("Filter activity instances based on presence of life cycle labels", height = 400))

}
