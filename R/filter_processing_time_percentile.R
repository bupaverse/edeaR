

filter_processing_time_percentile <- function(eventlog,
											  percentile_cut_off = NULL,
											  reverse = F) {



	case_durations <- processing_time(eventlog = eventlog, "case") %>% arrange(processing_time)

	colnames(case_durations)[colnames(case_durations)==case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog)==case_id(eventlog)] <- "case_classifier"


	case_selection <- case_durations$case_classifier[1:(nrow(case_durations)*percentile_cut_off)]

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