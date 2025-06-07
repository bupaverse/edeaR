rework_base <- function(eventlog) {

	min_order <- NULL
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
		as_tibble() %>%
		rename("case_classifier" = !!case_id_(eventlog),
			   "event_classifier" = !!activity_id_(eventlog),
			   "timestamp_classifier" = !!timestamp_(eventlog),
			   "aid" = !!activity_instance_id_(eventlog),
			   "resource_classifier" = !!resource_id_(eventlog)) %>%
		as.data.table %>%
		.[, .(timestamp = min(timestamp_classifier), min_order = min(.order)), .(case_classifier, aid, event_classifier, resource_classifier)] %>%
		.[order(timestamp, min_order), .SD , by =  .(case_classifier)] %>%
		.[, next_activity := shift(event_classifier, type='lead'), .(case_classifier)] %>%
		.[, same_activity := shift(event_classifier == next_activity, type='lag')] %>%
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
