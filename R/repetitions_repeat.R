repeat_repetitions <- function(eventlog) {

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
		.[, .(length = n_distinct(activity_group), nr_of_resources = n_distinct(resource_classifier), resource_classifier = first(resource_classifier)),
		  .(case_classifier, event_classifier, trace_length, activity_frequency)]%>%
		.[nr_of_resources == 1 &  length > 1]	%>%
		.[, length := length -1] %>%
		.[, .(case_classifier, event_classifier, trace_length, activity_frequency, length, resource_classifier)] %>%
		as.data.frame() -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)
	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)
	colnames(r)[colnames(r) == "resource_classifier"] <- resource_id(eventlog)

	return(r)
}


repeat_repetitions_case <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	cases <- eventlog[,case_id(eventlog)] %>% unique


	eventlog %>%
		repeat_repetitions() %>%
		group_by(!!case_id_(eventlog)) %>%
		summarize(absolute = n(),
				  trace_length = first(trace_length)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length)  %>% merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative))
}
repeat_repetitions_size_case <- function(eventlog) {
	eventlog %>%
		repeat_repetitions()  %>%
		group_by_(case_id(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!case_id_(eventlog), number_of_selfloops, everything())

}

repeat_repetitions_log <- function(eventlog) {
	absolute <- NULL
	eventlog %>%
		repeat_repetitions_case -> raw

	raw %>%
		pull(absolute) %>%
		summary_statistics -> output


	attr(output, "raw") <- raw
	return(output)
}
repeat_repetitions_size_log <- function(eventlog) {

	eventlog %>%
		repeat_repetitions -> raw

	raw %>%
		pull(length) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw

	return(output)

}


repeat_repetitions_activity <- function(eventlog) {
	absolute <- NULL
	relative <- NULL
	absolute_frequency <- NULL

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	eventlog %>%
		repeat_repetitions() %>%
		group_by(!!activity_id_(eventlog)) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) %>%
		merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative))
}
repeat_repetitions_size_activity <- function(eventlog) {

	absolute <- NULL
	relative <- NULL
	relative_frequency <- NULL
	absolute_frequency <- NULL

	activities <- eventlog %>% activities

	eventlog %>%
		repeat_repetitions() %>%
		group_by(!!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency)
}


repeat_repetitions_resource <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL
	absolute <- NULL
	relative <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		repeat_repetitions()  %>%
		group_by(!!resource_id_(eventlog)) %>%
		summarize(absolute = n())  %>%
		merge(resources, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency)

}
repeat_repetitions_size_resource <- function(eventlog) {

	absolute <- NULL
	relative <- NULL
	relative_frequency <- NULL
	absolute_frequency <- NULL

	resources <- eventlog %>% resources

	eventlog %>%
		repeat_repetitions() %>%
		group_by(!!resource_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!resource_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(resources, . ) %>%
		rename(relative_resource_frequency = relative_frequency) %>%
		select(-absolute_frequency)

}


repeat_repetitions_resource_activity <- function(eventlog) {

	absolute_frequency <- NULL
	absolute <- NULL
	relative_frequency <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		repeat_repetitions() %>%
		group_by(!!activity_id_(eventlog), !!resource_id_(eventlog)) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency)
}

repeat_repetitions_size_resource_activity <- function(eventlog) {
	first_resource <- NULL
	absolute_frequency <- NULL

	resources_activities <- eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(!!activity_instance_id(eventlog))) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)

	eventlog %>%
		repeat_repetitions() %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!resource_id_(eventlog), !!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(resources_activities, . )
}
