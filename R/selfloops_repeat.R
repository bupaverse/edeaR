# REPEAT SELFLOOPS

repeat_selfloops <- function(eventlog) {

	.N <- NULL
	case_classifier <- NULL
	resource_classifier <- NULL
	event_classifier <- NULL
	activity_group <- NULL
	nr_of_resources <- NULL
	t_length <- NULL

	eventlog %>%
		rework_base  %>%
		rename_("case_classifier" = case_id(eventlog),
				"event_classifier" = activity_id(eventlog),
				"resource_classifier" = resource_id(eventlog)) %>%
		as.data.table %>%
		.[, trace_length := .N, by = .(case_classifier)] %>%
		.[, activity_frequency := .N, by = .(event_classifier)] %>%
		.[, .(t_length = .N, nr_of_resources = n_distinct(resource_classifier), resource_classifier = first(resource_classifier)),
		  .(case_classifier, activity_group, event_classifier, trace_length, activity_frequency)] %>%
		.[nr_of_resources == 1 &  t_length > 1] %>%
		.[, length := t_length -1] %>%
		.[, .(case_classifier, event_classifier, resource_classifier, trace_length, activity_frequency, length)] %>%
		as.data.frame -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)
	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)
	colnames(r)[colnames(r) == "resource_classifier"] <- resource_id(eventlog)

	return(r)
}


#EACH LEVEL | NUMBER AND SIZE

repeat_selfloops_case <- function(eventlog) {

	absolute <- NULL
	cases <- eventlog[,case_id(eventlog)] %>% unique

	eventlog %>%
		repeat_selfloops %>%
		group_by(!!case_id_(eventlog), trace_length) %>%
		summarize(absolute = n()) %>%
		merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length)

}

repeat_selfloops_size_case <- function(eventlog) {
	eventlog %>%
		repeat_selfloops  %>%
		group_by(!!case_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!case_id_(eventlog), number_of_selfloops, everything())

}

repeat_selfloops_log <- function(eventlog) {
	absolute <- NULL
	eventlog %>%
		repeat_selfloops_case -> raw

	raw %>%
		pull(absolute) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	return(output)
}
repeat_selfloops_size_log <- function(eventlog, raw = F) {
	eventlog %>%
		repeat_selfloops -> raw

	raw %>%
		pull(length) %>%
		summary_statistics -> output
	attr(output, "raw") <- raw
	return(output)

}


repeat_selfloops_activity <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	eventlog %>%
		repeat_selfloops() %>%
		group_by(!!activity_id_(eventlog)) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) %>%
		merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative))
}
repeat_selfloops_size_activity <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL

	activities <- eventlog %>% activities

	eventlog %>%
		repeat_selfloops() %>%
		group_by(!!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency)

}


repeat_selfloops_resource <- function(eventlog) {
	absolute <- NULL
	resource <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		repeat_selfloops() %>%
		group_by(!!resource_id_(eventlog)) %>%
		summarize(absolute = n()) %>%
		merge(resources, all.y = TRUE) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency)

}
repeat_selfloops_size_resource <- function(eventlog) {
	absolute_frequency <- NULL
	relative_frequency <- NULL


	resources <- eventlog %>% resources

	eventlog %>%
		repeat_selfloops() %>%
		group_by(!!resource_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!resource_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(resources, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency)

}

repeat_selfloops_resource_activity <- function(eventlog) {

	absolute <- NULL
	relative <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		repeat_selfloops() %>%
		group_by(!!activity_id_(eventlog), !!resource_id_(eventlog)) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency)

}
repeat_selfloops_size_resource_activity <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL

	resources_activities <- eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(!!activity_instance_id_(eventlog))) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)

	eventlog %>%
		repeat_selfloops() %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog))  %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!activity_id_(eventlog), !!resource_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(resources_activities, . )

}
