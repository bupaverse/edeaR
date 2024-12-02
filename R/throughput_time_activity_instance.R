
throughput_time_activity_instance <- function(log, units, work_schedule = NULL) {

	s <- NULL
	elapsed <- NULL

	timestamp_classifier <- NULL
	activity_instance_classifier <- activity_instance_id(log)
	colnames(log)[colnames(log) == timestamp(log)] <- "timestamp_classifier"
	colnames(log)[colnames(log) == activity_instance_classifier] <- "activity_instance_classifier"

	e <- log %>%
		as.data.table %>%
		.[, .(s = min(timestamp_classifier),
			  e = max(timestamp_classifier)), .(activity_instance_classifier)]
	colnames(e)[colnames(e) == "activity_instance_classifier"] <- activity_instance_classifier

	intervals <- as.data.frame(e)

	if(is.null(work_schedule)) {
		intervals %>%
			mutate(throughput_time = difftime(e, s, units = units)) %>%
			select(-s, -e) -> output
	} else {

		calculate_work_schedule_times(intervals, work_schedule, units) %>%
			select(-s, -e) %>%
			rename(throughput_time = elapsed) -> output
	}

	attr(output, "units") <- attr(output[["throughput_time"]], "units")
	return(output)
}
