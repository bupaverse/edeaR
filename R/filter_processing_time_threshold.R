

filter_processing_time_threshold <- function(eventlog,
											 lower_threshold = NULL,
											 upper_threshold = NULL,
											 reverse = F,
											 units)
{


	if(is.null(lower_threshold))
		lower_threshold <- -Inf
	if(is.null(upper_threshold))
		upper_threshold <- Inf


	case_durations <- processing_time(eventlog = eventlog, "case", units = units)
	colnames(case_durations)[colnames(case_durations)==case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog)==case_id(eventlog)] <- "case_classifier"

	case_selection <- filter(case_durations, processing_time >= lower_threshold, processing_time <= upper_threshold)$case_classifier
	if(reverse == FALSE)
		f_eventlog <- filter(eventlog, case_classifier %in% case_selection)
	else
		f_eventlog <- filter(eventlog, !(case_classifier %in% case_selection))

	colnames(f_eventlog)[colnames(f_eventlog)=="case_classifier"] <- case_id(eventlog)

	output <- eventlog(f_eventlog,
					   activity_id = activity_id(eventlog),
					   case_id = case_id(eventlog),
					   timestamp =timestamp(eventlog),
					   lifecycle_id = lifecycle_id(eventlog),
					   activity_instance_id = activity_instance_id(eventlog))

	return(output)

}
