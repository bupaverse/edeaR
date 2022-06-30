

throughput_time_case <- function(eventlog, units, work_schedule = NULL) {


	s <- NULL
	elapsed <- NULL

	timestamp_classifier <- NULL
	case_classifier <- case_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == case_classifier] <- "case_classifier"

	e <- eventlog %>%
		as.data.table %>%
		.[, .(s = min(timestamp_classifier),
			  e = max(timestamp_classifier)), .(case_classifier)]
	colnames(e)[colnames(e) == "case_classifier"] <- case_classifier

	intervals <- as.data.frame(e)

	if(is.null(work_schedule)) {
		intervals %>%
			mutate(throughput_time = difftime(e, s, units = units)) %>%
			select(-s, -e)
	} else {

		calculate_work_schedule_times(intervals, work_schedule, units) %>%
			select(-s, -e) %>%
			rename(throughput_time = elapsed)





	}










}
