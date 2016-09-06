
resource_frequency_resource_activity <- function(eventlog, include_zeros) {
	resource_classifier <- resource_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)
	event_classifier <- activity_id(eventlog)


	all_pairs <- expand.grid(as.vector(t(select(resources(eventlog),1))), as.vector(t(select(activities(eventlog),1))))

	colnames(all_pairs) <- c(resource_id(eventlog), activity_id(eventlog))

	if(!include_zeros){
		eventlog %>%
			group_by_(resource_classifier, event_classifier, activity_instance_classifier) %>%
			summarize() %>%
			summarize(absolute = n()) %>%
			group_by_(resource_classifier) %>%
			mutate(relative_resource = absolute/sum(absolute)) %>%
			group_by_(event_classifier) %>%
			mutate(relative_activity = absolute/sum(absolute)) %>%
			arrange(-absolute)-> r
	}
	else{
		eventlog %>%
			group_by_(resource_classifier, event_classifier, activity_instance_classifier) %>%
			summarize() %>%
			summarize(absolute = n()) %>%
			right_join(all_pairs) %>%
			mutate(absolute = ifelse(is.na(absolute),0,absolute)) %>%
			group_by_(resource_classifier) %>%
			mutate(relative_resource = absolute/sum(absolute)) %>%
			group_by_(event_classifier) %>%
			mutate(relative_activity = absolute/sum(absolute)) %>%
			arrange(-absolute)-> r
	}
	return(r)
}
