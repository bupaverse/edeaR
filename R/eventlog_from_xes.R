#' @title eventlog_from_xes
#'
#' @export eventlog_from_xes

eventlog_from_xes <- function(xesfile = file.choose()){

	log <- csv_from_xes(xesfile)

	colnames(log) <- gsub(":","_", colnames(log))

	log <- arrange(log, trace.concept_name, event.concept_name, event.time_timestamp)

	log$activity_instance[1] <- 1

	for(i in 2:nrow(log))

		if(log$event.lifecycle_transition[i-1] %in% c("autoskip","manualskip","complete","withdraw","abort_activity","abort_case"))
			log$activity_instance[i] <- log$activity_instance[i-1] + 1
		else
			log$activity_instance[i] <- log$activity_instance[i-1]


	elog <- eventlog(eventlog = log,
					 case_id = "trace.concept_name",
					 activity_id = "event.concept_name",
					 activity_instance_id = "activity_instance",
					 life_cycle_id = "event.lifecycle_transition",
					 timestamp = "event.time_timestamp")
	return(elog)
}
