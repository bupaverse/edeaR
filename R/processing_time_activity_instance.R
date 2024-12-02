
processing_time_activity_instance <- function(log, units, work_schedule) {

	s <- NULL
	e <- NULL
	timestamp_classifier <- NULL
	activity_id_identifier <- NULL
	lifecycle_id_identifier <- NULL
	next_TS <- NULL
	activate <- NULL
	dur <- NULL
	elapsed <- NULL


	renamed_log <- log

	colnames(renamed_log)[colnames(renamed_log) == timestamp(log)] <- "timestamp_classifier"
	colnames(renamed_log)[colnames(renamed_log) == activity_instance_id(log)] <- "activity_id_identifier"
	colnames(renamed_log)[colnames(renamed_log) == lifecycle_id(log)] <- "lifecycle_id_identifier"

	renamed_log %>%
		as.data.table() %>%
		.[, "activate" := fcase(lifecycle_id_identifier %in% c("start","resume"),  1L,
								default = -1L)] -> dt

	setorderv(dt, cols = c("timestamp_classifier", ".order"))

	dt[,":="("next_TS" = shift(timestamp_classifier, type = "lead")),
	   by = "activity_id_identifier"][,
	   							   "dur" := difftime(next_TS, timestamp_classifier, units = units)][
	   							   	activate == 1L & !is.na(dur)][
	   							   		, .("processing_time" = sum(dur)), by = "activity_id_identifier"] -> output
	output <- as.data.frame(output)

	colnames(output)[colnames(output) == "activity_id_identifier"] <- activity_instance_id(log)

	if(!is.null(work_schedule))
		cli::cli_warn("Work schedule currently not supporting for processing time")
	# if(is.null(work_schedule)) {
	# 	intervals %>%
	# 		mutate(processing_time = difftime(e, s, units = units)) %>%
	# 		select(-s, -e) -> output
	# } else {
	#
	# 	calculate_work_schedule_times(intervals, work_schedule, units) %>%
	# 		select(-s, -e) %>%
	# 		rename(processing_time = elapsed) -> output
	#
	# }

	attr(output, "units") <- attr(output[["processing_time"]], "units")
	return(output)
}



