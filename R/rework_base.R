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
