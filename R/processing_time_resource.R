
processing_time_resource <- function(eventlog,
									 units,
									 work_schedule) {

	expand.grid(case_labels(eventlog),
				resource_labels(eventlog)) %>%
		setNames(c(case_id(eventlog), resource_id(eventlog))) -> case_resource_list

	eventlog %>%
		processing_time_activity_instance(units = units, work_schedule = work_schedule) %>%
		group_by(!!resource_id_(eventlog), !!case_id_(eventlog)) %>%
		summarize(processing_time = sum(processing_time)) %>%
		merge(case_resource_list, all.y = TRUE)%>%
		mutate(processing_time = ifelse(is.na(processing_time),0,processing_time)) -> raw

	raw %>%
		group_by(!!resource_id_(eventlog)) %>%
		grouped_summary_statistics("processing_time") -> output

	attr(output, "raw") <- raw

	return(output)

}
