rework_base <- function(eventlog) {

	event_classifier <- NULL
	case_classifier <- NULL
	timestamp_classifier <- NULL
	aid <- NULL
	resource_classifier <- NULL
	.SD <- NULL
	next_activity <- NULL
	same_activity <- NULL
	activity_group <- NULL

	eventlog %>%
		rename_("case_classifier" = case_id(eventlog),
				"event_classifier" = activity_id(eventlog),
				"timestamp_classifier" = timestamp(eventlog),
				"aid" = activity_instance_id(eventlog),
				"resource_classifier" = resource_id(eventlog)) %>%
		as.data.table %>%
		.[, .(timestamp = min(timestamp_classifier), min_order = min(.order)), .(case_classifier, aid, event_classifier, resource_classifier)] %>%
		.[order(timestamp, min_order), .SD , by =  .(case_classifier)] %>%
		.[,next_activity := lead(event_classifier), .(case_classifier)] %>%
		.[, same_activity := lag(event_classifier == next_activity)] %>%
		.[, same_activity := ifelse(is.na(same_activity), FALSE, same_activity)]  %>%
		.[, activity_group := paste(case_classifier, cumsum(!same_activity), sep = "-")] %>%
		.[,.(case_classifier, aid, event_classifier, resource_classifier, activity_group)] %>%
		as.data.frame -> r


	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)
	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)
	colnames(r)[colnames(r) == "resource_classifier"] <- resource_id(eventlog)
	colnames(r)[colnames(r) == "aid"] <- activity_instance_id(eventlog)

	return(r)
}

#selfloops
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
redo_selfloops <- function(eventlog) {

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
		.[nr_of_resources > 1 &  t_length > 1] %>%
		.[, length := t_length -1] %>%
		.[, .(case_classifier, event_classifier, first_resource, last_resource, trace_length, activity_frequency, length)] %>%
		as.data.frame -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)
	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	return(r)
}

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

redo_selfloops_case <- function(eventlog) {

	absolute <- NULL
	relative <- NULL

	cases <- eventlog[,case_id(eventlog)] %>% unique

	eventlog %>%
		redo_selfloops %>%
		group_by(!!case_id_(eventlog)) %>%
		summarize(absolute = n(),
				  trace_length = first(trace_length)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length) %>% merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
redo_selfloops_size_case <- function(eventlog) {
	eventlog %>%
		redo_selfloops  %>%
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
redo_selfloops_log <- function(eventlog) {
	absolute <- NULL
	eventlog %>%
		redo_selfloops_case -> raw
	raw %>%
		pull(absolute) %>%
		summary_statistics -> output

	attr(output, "raw") <- raw
	return(output)
}
redo_selfloops_size_log <- function(eventlog, raw = F) {
	eventlog %>%
		redo_selfloops -> raw

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
redo_selfloops_activity <- function(eventlog) {

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	absolute <- NULL
	relative <- NULL

	eventlog %>%
		redo_selfloops() %>%
		group_by(!!activity_id_(eventlog)) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) %>%
		merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative))
}
redo_selfloops_size_activity <- function(eventlog) {

	relative_frequency <- NULL
	absolute_frequency <- NULL

	activities <- eventlog %>% activities

	eventlog %>%
		redo_selfloops() %>%
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

redo_selfloops_resource <- function(eventlog) {

	first_resource <- NULL
	absolute <- NULL
	relative <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		redo_selfloops() %>%
		group_by(first_resource) %>%
		summarize(absolute = n())  %>%
		merge(resources, all.y = T, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency)

}
redo_selfloops_size_resource <- function(eventlog) {

	absolute_frequency <- NULL
	relative_frequency <- NULL
	first_resource <- NULL

	resources <- eventlog %>% resources

	eventlog %>%
		redo_selfloops() %>%
		group_by(first_resource) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(first_resource, number_of_selfloops, everything()) %>%
		merge(resources, . , by.x = resource_id(eventlog), by.y = "first_resource") %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r

	colnames(r)[colnames(r) == resource_id(eventlog)] <- "first_resource"
	return(r)
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
redo_selfloops_resource_activity <- function(eventlog) {

	absolute <- NULL
	first_resource <- NULL
	relative_frequency <- NULL
	absolute_frequency <- NULL

	resources <- resources(eventlog)

	eventlog %>%
		redo_selfloops()  %>%
		group_by(!!activity_id_(eventlog), first_resource) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency)

}
redo_selfloops_size_resource_activity <- function(eventlog) {

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
		redo_selfloops() %>%
		group_by(first_resource, !!activity_id_(eventlog)) %>%
		grouped_summary_statistics("length", number_of_selfloops = n()) %>%
		select(first_resource, !!activity_id_(eventlog), number_of_selfloops, everything()) %>%
		merge(resources_activities, . , by.x = c(resource_id(eventlog), activity_id(eventlog)), by.y = c("first_resource", activity_id(eventlog)))

}


#repetitions
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




