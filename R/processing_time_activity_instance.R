
processing_time_activity_instance <- function(log, units, work_schedule) {

	s <- NULL
	e <- NULL
	timestamp_classifier <- NULL
	activity_id_identifier <- NULL
	elapsed <- NULL


	renamed_log <- log

	colnames(renamed_log)[colnames(renamed_log) == timestamp(log)] <- "timestamp_classifier"
	colnames(renamed_log)[colnames(renamed_log) == activity_instance_id(log)] <- "activity_id_identifier"

	e <- renamed_log %>%
		as.data.table %>%
		.[, .(s = min(timestamp_classifier),
			  e = max(timestamp_classifier)), .(activity_id_identifier)]

	colnames(e)[colnames(e) == "activity_id_identifier"] <- activity_instance_id(log)

	intervals <- as.data.frame(e)


	if(is.null(work_schedule)) {
		intervals %>%
			mutate(processing_time = difftime(e, s, units = units)) %>%
			select(-s, -e) -> output
	} else {

		calculate_work_schedule_times(intervals, work_schedule, units) %>%
			select(-s, -e) %>%
			rename(processing_time = elapsed) -> output

	}
	output

}



