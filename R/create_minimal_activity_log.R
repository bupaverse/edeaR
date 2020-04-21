
create_minimal_activity_log <- function(eventlog) {
	eventlog %>%
		group_by(!!case_id_(eventlog),
				 !!activity_id_(eventlog),
				 !!activity_instance_id_(eventlog)) %>%
		summarize(resource_identifier = first(!!resource_id_(eventlog)),
				  time = min(!!timestamp_(eventlog)),
				  min_order = min(.order))
}
