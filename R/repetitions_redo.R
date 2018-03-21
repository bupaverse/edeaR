
redo_repetitions <- function(eventlog) {
	.N <- NULL
	case_classifier <- NULL
	event_classifier <- NULL
	resource_classifier <- NULL
	first_resource <- NULL
	last_resource <- NULL
	activity_group <- NULL
	nr_of_resources <- NULL
	t_length <- NULL

	eventlog %>%
		rework_base %>%
		rename_("case_classifier" = case_id(eventlog),
				"event_classifier" = activity_id(eventlog),
				"resource_classifier" = resource_id(eventlog)) %>%
		as.data.table %>%
		.[, trace_length := .N, by = .(case_classifier)] %>%
		.[, activity_frequency := .N, by = .(event_classifier)] %>%
		.[, .(length = n_distinct(activity_group),
			  nr_of_resources = n_distinct(resource_classifier),
			  first_resource = data.table::first(resource_classifier),
			  last_resource = data.table::last(resource_classifier)),
		  .(case_classifier, event_classifier, trace_length, activity_frequency)]%>%
		.[nr_of_resources > 1 &  length > 1]	%>%
		.[, length := length -1] %>%
		.[, .(case_classifier, event_classifier, trace_length, activity_frequency, length, first_resource, last_resource)] %>%
		as.data.frame() -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)
	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	return(r)
}


redo_repetitions_case <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	cases <- eventlog[,case_id(eventlog)] %>% unique

	eventlog %>%
		redo_repetitions %>%
		group_by(!!case_id_(eventlog)) %>%
		summarize(absolute = n(),
				  trace_length = first(trace_length)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length) %>%
		merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative))

}
redo_repetitions_size_case <- function(eventlog) {
	eventlog %>%
		redo_repetitions()  %>%
		group_by(!!case_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!case_id_(eventlog), number_of_selfloops, everything())
}


redo_repetitions_log <- function(eventlog) {
	absolute <- NULL
	eventlog %>%
		redo_repetitions_case -> raw

	raw %>%
		pull(absolute) %>%
		summary_statistics -> output


	attr(output, "raw") <- raw
	return(output)
}
redo_repetitions_size_log <- function(eventlog) {
	eventlog %>%
		redo_repetitions -> raw

	raw %>%
		pull(length) %>%
		summary_statistics -> output


	attr(output, "raw") <- raw

	return(output)

}

redo_repetitions_activity <- function(eventlog) {

	absolute <- NULL
	relative <- NULL
	absolute_frequency <- NULL

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	eventlog %>%
		redo_repetitions() %>%
		group_by(!!activity_id_(eventlog)) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) %>%
		merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative))
}
redo_repetitions_size_activity <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL

	activities <- eventlog %>% activities

	eventlog %>%
		redo_repetitions %>%
		group_by(!!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency)
}

redo_repetitions_resource <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL
	absolute <- NULL
	relative <- NULL
	first_resource <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		redo_repetitions() %>%
		group_by(first_resource) %>%
		summarize(absolute = n())  %>%
		merge(resources, all.y = T, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency)

}
redo_repetitions_size_resource <- function(eventlog) {

	resources <- eventlog %>% resources

	relative_frequency <- NULL
	absolute_frequency <- NULL
	first_resource <- NULL


	eventlog %>%
		redo_repetitions() %>%
		group_by(first_resource) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(first_resource, number_of_selfloops, everything()) %>%
		merge(resources, . , by.x = resource_id(eventlog), by.y = "first_resource") %>%
		rename(relative_resource_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r

	colnames(r)[colnames(r) == resource_id(eventlog)] <- "first_resource"
	return(r)
}

redo_repetitions_resource_activity <- function(eventlog) {

	absolute <- NULL
	relative_frequency <- NULL
	absolute_frequency <- NULL
	first_resource <- NULL


	resources <- resources(eventlog)

	eventlog %>%
		redo_repetitions() %>%
		group_by(!!activity_id_(eventlog), first_resource) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency)
}
redo_repetitions_size_resource_activity <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL
	first_resource <- NULL


	resources_activities <- eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(!!activity_instance_id_(eventlog))) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)

	eventlog %>%
		redo_repetitions()  %>%
		group_by(first_resource, !!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(first_resource, !!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(resources_activities, . , by.x = c(resource_id(eventlog), activity_id(eventlog)), by.y = c("first_resource", activity_id(eventlog)) )

}


