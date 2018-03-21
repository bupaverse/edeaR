
# REDO SELFLOOPS BASE

all_selfloops <- function(eventlog) {

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
		rework_base -> r

	r %>%
		rename_("case_classifier" = case_id(eventlog),
				"event_classifier" = activity_id(eventlog),
				"resource_classifier" = resource_id(eventlog)) %>%
		as.data.table %>%
		.[, trace_length := .N, by = .(case_classifier)] %>%
		.[, activity_frequency := .N, by = .(event_classifier)] %>%
		.[, .(t_length = .N,
			  nr_of_resources = n_distinct(resource_classifier),
			  first_resource = first(resource_classifier),
			  last_resource = last(resource_classifier)),
		  .(case_classifier, activity_group, event_classifier, trace_length, activity_frequency)] %>%
		.[  t_length > 1] %>%
		.[, length := t_length -1] %>%
		.[, .(case_classifier, event_classifier, first_resource, last_resource, trace_length, activity_frequency, length)] %>%
		as.data.frame -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)
	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	return(r)
}

# EACH LEVEL | NUMBER & SIZE

all_selfloops_case <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	cases <- eventlog[,case_id(eventlog)] %>% unique

	eventlog %>%
		all_selfloops %>%
		group_by(!!case_id_(eventlog)) %>%
		summarize(absolute = n(),
				  trace_length = first(trace_length)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length) %>% merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
all_selfloops_size_case <- function(eventlog) {
	eventlog %>%
		all_selfloops  %>%
		group_by(!!case_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!case_id_(eventlog), number_of_selfloops, everything())
}


all_selfloops_log <- function(eventlog) {
	absolute <- NULL
	eventlog %>%
		all_selfloops_case -> raw
	raw %>%
		pull(absolute) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	return(output)
}
all_selfloops_size_log <- function(eventlog, raw = F) {
	eventlog %>%
		all_selfloops -> raw

	raw %>%
		pull(length) %>%
		summary_statistics -> output
	attr(output, "raw") <- raw
	return(output)

}

all_selfloops_activity <- function(eventlog) {

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	absolute <- NULL
	relative <- NULL

	eventlog %>%
		all_selfloops() %>%
		group_by(!!activity_id_(eventlog)) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) %>%
		merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative))
}
all_selfloops_size_activity <- function(eventlog) {

	relative_frequency <- NULL
	absolute_frequency <- NULL

	activities <- eventlog %>% activities

	eventlog %>%
		all_selfloops() %>%
		group_by(!!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(!!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency)
}


all_selfloops_resource <- function(eventlog) {

	first_resource <- NULL
	absolute <- NULL
	relative <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		all_selfloops() %>%
		group_by(first_resource) %>%
		summarize(absolute = n())  %>%
		merge(resources, all.y = T, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency)

}
all_selfloops_size_resource <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL
	first_resource <- NULL

	resources <- eventlog %>% resources

	eventlog %>%
		all_selfloops() %>%
		group_by(first_resource) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(first_resource, number_of_selfloops, everything()) %>%
		merge(resources, . , by.x = resource_id(eventlog), by.y = "first_resource") %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r

	colnames(r)[colnames(r) == resource_id(eventlog)] <- "first_resource"
	return(r)
}


all_selfloops_resource_activity <- function(eventlog) {

	absolute <- NULL
	first_resource <- NULL
	relative_frequency <- NULL
	absolute_frequency <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		all_selfloops()  %>%
		group_by(!!activity_id_(eventlog), first_resource) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency)

}
all_selfloops_size_resource_activity <- function(eventlog) {

	first_resource <- NULL
	relative_frequency <- NULL
	absolute_frequency <- NULL

	resources_activities <- eventlog %>%
		group_by(!!resource_id_(eventlog), !!activity_id_(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(!!activity_instance_id_(eventlog))) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)


	eventlog %>%
		all_selfloops() %>%
		group_by(first_resource, !!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(first_resource, !!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(resources_activities, . , by.x = c(resource_id(eventlog), activity_id(eventlog)), by.y = c("first_resource", activity_id(eventlog)))

}
