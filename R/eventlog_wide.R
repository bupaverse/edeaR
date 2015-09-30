eventlog_wide <- function(eventlog) {

	library(tidyr)
	library(lubridate)

	colnames(eventlog)[colnames(eventlog)== timestamp(eventlog)] <- "timestamp_classifier"
	colnames(eventlog)[colnames(eventlog) == case_id(eventlog)] <- "case_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_id(eventlog)] <- "event_classifier"
	colnames(eventlog)[colnames(eventlog) == life_cycle_id(eventlog)] <- "life_cycle_classifier"
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"


	eventlog$timestamp_classifier <- as.character(eventlog$timestamp_classifier)
	r <- spread(eventlog, life_cycle_classifier, timestamp_classifier)
	r$start <- ymd_hms(r$start)
	r$complete <- ymd_hms(r$complete)
	return(r)


}
