rework_base <- function(eventlog) {

	eventlog %>%
		rename_("case_classifier" = case_id(eventlog),
				"event_classifier" = activity_id(eventlog),
				"timestamp_classifier" = timestamp(eventlog),
				"aid" = activity_instance_id(eventlog),
				"resource_classifier" = resource_id(eventlog)) %>%
		as.data.table %>%
		.[, .(timestamp = min(timestamp_classifier)), .(case_classifier, aid, event_classifier, resource_classifier)] %>%
		.[order(timestamp), .SD , by =  .(case_classifier)] %>%
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

	eventlog %>%
		rework_base -> r

	r %>%
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

	eventlog %>%
		repeat_selfloops -> r

	cases <- eventlog[,case_id(eventlog)] %>% unique

	r %>%
		group_by_(case_id(eventlog), "trace_length") %>%
		summarize(absolute = n()) %>%
		merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length) -> r


	return(r)
}
repeat_selfloops_size_case <- function(eventlog) {
	eventlog %>%
		repeat_selfloops  %>%
		group_by_(case_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) -> r
	return(r)
}
redo_selfloops_case <- function(eventlog) {

	eventlog %>%
		redo_selfloops -> r

	cases <- eventlog[,case_id(eventlog)] %>% unique

	colnames(r)[colnames(r) == case_id(eventlog)] <- "case_classifier"

	r %>%
		group_by(case_classifier) %>%
		summarize(absolute = n(),
				  trace_length = first(trace_length)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length) -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)

	r %>% merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
redo_selfloops_size_case <- function(eventlog) {
	eventlog %>%
		redo_selfloops  %>%
		group_by_(case_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) -> r
	return(r)
}

repeat_selfloops_log <- function(eventlog) {
	eventlog %>%
		repeat_selfloops_case -> r

	output <- summary_statistics(r$absolute)

	attr(output, "raw") <- r
	return(output)
}
repeat_selfloops_size_log <- function(eventlog, raw = F) {
	eventlog %>%
		repeat_selfloops -> r


	output <- summary_statistics(r$length)
	attr(output, "raw") <- r
	return(output)

}
redo_selfloops_log <- function(eventlog) {
	eventlog %>%
		redo_selfloops_case -> r

	output <- summary_statistics(r$absolute)
	attr(output, "raw") <- r
	return(output)
}
redo_selfloops_size_log <- function(eventlog, raw = F) {
	eventlog %>%
		redo_selfloops -> r

	output <- summary_statistics(r$length)
	attr(output, "raw") <- r
	return(output)

}

repeat_selfloops_activity <- function(eventlog) {
	eventlog %>%
		repeat_selfloops() -> r

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	r %>%
		group_by(event_classifier) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	r %>% merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
repeat_selfloops_size_activity <- function(eventlog) {
	eventlog %>%
		repeat_selfloops() -> r

	activities <- eventlog %>% activities

	r %>%
		group_by_(activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r
	return(r)
}
redo_selfloops_activity <- function(eventlog) {
	eventlog %>%
		redo_selfloops() -> r

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	r %>%
		group_by(event_classifier) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	r %>% merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
redo_selfloops_size_activity <- function(eventlog) {
	eventlog %>%
		redo_selfloops() -> r

	activities <- eventlog %>% activities

	r %>%
		group_by_(activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r
	return(r)
}

repeat_selfloops_resource <- function(eventlog) {
	eventlog %>%
		repeat_selfloops() -> r

	colnames(r)[colnames(r) == resource_id(eventlog)] <- "resource_classifier"

	resources <- resources(eventlog)

	r %>%
		group_by(resource_classifier) %>%
		summarize(absolute = n()) -> r

	colnames(r)[colnames(r) == "resource_classifier"] <- resource_id(eventlog)

	r %>% merge(resources, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency) -> r

	return(r)

}
repeat_selfloops_size_resource <- function(eventlog) {
	eventlog %>%
		repeat_selfloops() -> r

	resources <- eventlog %>% resources

	r %>%
		group_by_(resource_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources, . ) %>%
		rename(relative_resource_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r
	return(r)
}
redo_selfloops_resource <- function(eventlog) {
	eventlog %>%
		redo_selfloops() -> r


	resources <- resources(eventlog)

	r %>%
		group_by(first_resource) %>%
		summarize(absolute = n()) -> r


	r %>% merge(resources, all.y = T, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency) -> r

	return(r)

}
redo_selfloops_size_resource <- function(eventlog) {
	eventlog %>%
		redo_selfloops() -> r

	resources <- eventlog %>% resources

	r %>%
		group_by(first_resource) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources, . , by.x = resource_id(eventlog), by.y = "first_resource") %>%
		rename(relative_resource_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r

	colnames(r)[colnames(r) == resource_id(eventlog)] <- "first_resource"
	return(r)
}

repeat_selfloops_resource_activity <- function(eventlog) {
	eventlog %>%
		repeat_selfloops() -> r

	resources <- resources(eventlog)

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"
	colnames(r)[colnames(r) == resource_id(eventlog)] <- "resource_classifier"

	r %>%
		group_by(event_classifier, resource_classifier) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources, by.x = "resource_classifier", by.y = resource_id(eventlog)) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)
	colnames(r)[colnames(r) == "resource_classifier"] <- resource_id(eventlog)

	return(r)
}
repeat_selfloops_size_resource_activity <- function(eventlog) {
	eventlog %>%
		repeat_selfloops() -> r

	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "aid"

	resources_activities <- eventlog %>%
		group_by_(resource_id(eventlog), activity_id(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(aid)) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)

	r %>%
		group_by_(resource_id(eventlog), activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources_activities, . ) -> r
	colnames(r)[colnames(r) == resource_id(eventlog)] <- "first_resource"

	return(r)
}
redo_selfloops_resource_activity <- function(eventlog) {
	eventlog %>%
		redo_selfloops() -> r

	resources <- resources(eventlog)

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"

	r %>%
		group_by(event_classifier, first_resource) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	return(r)
}
redo_selfloops_size_resource_activity <- function(eventlog) {

	eventlog %>%
		redo_selfloops() -> r

	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "aid"


	resources_activities <- eventlog %>%
		group_by_(resource_id(eventlog), activity_id(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(aid)) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)

	r %>%
		group_by_("first_resource", activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources_activities, . , by.x = c(resource_id(eventlog), activity_id(eventlog)), by.y = c("first_resource", activity_id(eventlog)) ) -> r
	return(r)

}


#repetitions
repeat_repetitions <- function(eventlog) {

	eventlog %>%
		rework_base -> r


	r %>%
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

	eventlog %>%
		rework_base -> r

	r %>%
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

	eventlog %>%
		repeat_repetitions() -> r

	cases <- eventlog[,case_id(eventlog)] %>% unique

	colnames(r)[colnames(r) == case_id(eventlog)] <- "case_classifier"

	r %>%
		group_by(case_classifier) %>%
		summarize(absolute = n(),
				  trace_length = first(trace_length)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length) -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)

	r %>% merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
repeat_repetitions_size_case <- function(eventlog) {
	eventlog %>%
		repeat_repetitions()  %>%
		group_by_(case_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) -> r
	return(r)
}
redo_repetitions_case <- function(eventlog) {

	eventlog %>%
		redo_repetitions -> r

	cases <- eventlog[,case_id(eventlog)] %>% unique

	colnames(r)[colnames(r) == case_id(eventlog)] <- "case_classifier"

	r %>%
		group_by(case_classifier) %>%
		summarize(absolute = n(),
				  trace_length = first(trace_length)) %>%
		mutate(relative = absolute/trace_length) %>%
		select(-trace_length) -> r

	colnames(r)[colnames(r) == "case_classifier"] <- case_id(eventlog)

	r %>% merge(cases, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
redo_repetitions_size_case <- function(eventlog) {
	eventlog %>%
		redo_repetitions()  %>%
		group_by_(case_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) -> r
	return(r)
}

repeat_repetitions_log <- function(eventlog) {
	eventlog %>%
		repeat_repetitions_case -> r

	summary_statistics(r$absolute) -> output

	attr(output, "raw") <- r
	return(output)
}
repeat_repetitions_size_log <- function(eventlog) {
	eventlog %>%
		repeat_repetitions -> r

	summary_statistics(r$length) -> output
	attr(output, "raw") <- r
	return(output)

}
redo_repetitions_log <- function(eventlog) {
	eventlog %>%
		redo_repetitions_case -> r

	summary_statistics(r$absolute) -> output
	attr(output, "raw") <- r
	return(output)
}
redo_repetitions_size_log <- function(eventlog) {
	eventlog %>%
		redo_repetitions -> r

	summary_statistics(r$length) -> output
	attr(output, "raw") <- r
	return(output)

}

repeat_repetitions_activity <- function(eventlog) {
	eventlog %>%
		repeat_repetitions() -> r

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	r %>%
		group_by(event_classifier) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	r %>% merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
repeat_repetitions_size_activity <- function(eventlog) {
	eventlog %>%
		repeat_repetitions() -> r

	activities <- eventlog %>% activities

	r %>%
		group_by_(activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r
	return(r)
}
redo_repetitions_activity <- function(eventlog) {
	eventlog %>%
		redo_repetitions() -> r

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"

	activities <- eventlog[,activity_id(eventlog)] %>% unique

	r %>%
		group_by(event_classifier) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		mutate(relative = absolute/activity_frequency) %>%
		select(-activity_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	r %>% merge(activities, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = ifelse(is.na(relative), 0, relative)) -> r

	return(r)
}
redo_repetitions_size_activity <- function(eventlog) {
	eventlog %>%
		redo_repetitions() -> r

	activities <- eventlog %>% activities

	r %>%
		group_by_(activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(activities, . ) %>%
		rename(relative_activity_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r
	return(r)
}

repeat_repetitions_resource <- function(eventlog) {
	eventlog %>%
		repeat_repetitions() -> r

	colnames(r)[colnames(r) == resource_id(eventlog)] <- "resource_classifier"

	resources <- resources(eventlog)

	r %>%
		group_by(resource_classifier) %>%
		summarize(absolute = n()) -> r

	colnames(r)[colnames(r) == "resource_classifier"] <- resource_id(eventlog)

	r %>% merge(resources, all.y = T) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency) -> r

	return(r)

}
repeat_repetitions_size_resource <- function(eventlog) {
	eventlog %>%
		repeat_repetitions() -> r

	resources <- eventlog %>% resources

	r %>%
		group_by_(resource_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources, . ) %>%
		rename(relative_resource_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r
	return(r)
}
redo_repetitions_resource <- function(eventlog) {
	eventlog %>%
		redo_repetitions() -> r


	resources <- resources(eventlog)

	r %>%
		group_by(first_resource) %>%
		summarize(absolute = n()) -> r


	r %>% merge(resources, all.y = T, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(absolute = ifelse(is.na(absolute), 0, absolute),
			   relative = absolute/absolute_frequency) %>%
		select(-absolute_frequency, -relative_frequency) -> r

	return(r)

}
redo_repetitions_size_resource <- function(eventlog) {
	eventlog %>%
		redo_repetitions() -> r

	resources <- eventlog %>% resources

	r %>%
		group_by(first_resource) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources, . , by.x = resource_id(eventlog), by.y = "first_resource") %>%
		rename(relative_resource_frequency = relative_frequency) %>%
		select(-absolute_frequency) -> r

	colnames(r)[colnames(r) == resource_id(eventlog)] <- "first_resource"
	return(r)
}

repeat_repetitions_resource_activity <- function(eventlog) {
	eventlog %>%
		repeat_repetitions() -> r

	resources <- resources(eventlog)

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"
	colnames(r)[colnames(r) == resource_id(eventlog)] <- "resource_classifier"

	r %>%
		group_by(event_classifier, resource_classifier) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources, by.x = "resource_classifier", by.y = resource_id(eventlog)) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)
	colnames(r)[colnames(r) == "resource_classifier"] <- resource_id(eventlog)

	return(r)
}
repeat_repetitions_size_resource_activity <- function(eventlog) {
	eventlog %>%
		repeat_repetitions() -> r

	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "aid"

	resources_activities <- eventlog %>%
		group_by_(resource_id(eventlog), activity_id(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(aid)) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)

	r %>%
		group_by_(resource_id(eventlog), activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources_activities, . ) -> r
	colnames(r)[colnames(r) == resource_id(eventlog)] <- "first_resource"

	return(r)
}
redo_repetitions_resource_activity <- function(eventlog) {
	eventlog %>%
		redo_repetitions() -> r

	resources <- resources(eventlog)

	colnames(r)[colnames(r) == activity_id(eventlog)] <- "event_classifier"

	r %>%
		group_by(event_classifier, first_resource) %>%
		summarize(absolute = n(),
				  activity_frequency = first(activity_frequency)) %>%
		merge(resources, by.x = "first_resource", by.y = resource_id(eventlog)) %>%
		mutate(relative_activity = absolute/activity_frequency,
			   relative_resource = absolute/absolute_frequency) %>%
		select(-activity_frequency, -absolute_frequency, -relative_frequency) -> r

	colnames(r)[colnames(r) == "event_classifier"] <- activity_id(eventlog)

	return(r)
}
redo_repetitions_size_resource_activity <- function(eventlog) {

	eventlog %>%
		redo_repetitions() -> r

	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "aid"


	resources_activities <- eventlog %>%
		group_by_(resource_id(eventlog), activity_id(eventlog)) %>%
		summarize(absolute_frequency = dplyr::n_distinct(aid)) %>%
		ungroup() %>%
		mutate(relative_frequency = absolute_frequency/sum(absolute_frequency)) %>%
		select(-absolute_frequency)

	r %>%
		group_by_("first_resource", activity_id(eventlog)) %>%
		summarize(number_of_selfloops = n(),
				  min = min(length),
				  q1 = quantile(length, 0.25),
				  mean = mean(length),
				  median = median(length),
				  q3 = quantile(length, 0.75),
				  max = max(length),
				  st_dev = sd(length),
				  iqr = q3 - q1) %>%
		merge(resources_activities, . , by.x = c(resource_id(eventlog), activity_id(eventlog)), by.y = c("first_resource", activity_id(eventlog)) ) -> r
	return(r)

}




