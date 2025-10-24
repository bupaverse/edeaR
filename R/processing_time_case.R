..cols <- NULL


processing_time_case <- function(log, units, work_schedule) {

	case_id <- log %>% case_id_()
	log %>%
		distinct(!!case_id, !!activity_instance_id_(log)) -> dict

	log %>%
		processing_time_activity_instance(units = units,
										  work_schedule = work_schedule) -> raw

	# Store time units, because dplyr transformations remove the attributes.
	time_units <- attr(raw, "units")

	dict <- dict %>%
		full_join(raw, by = activity_instance_id(log))
	# Use data.table to summarise
	dict <- as.data.table(dict)
	cols <- c(rlang::as_name(case_id),'processing_time')
	dict <- dict[, ..cols]
	dict <- dict[, .(processing_time = sum(processing_time, na.rm=TRUE)),
				   by=c(cols[1])] # by column needs to be a vector

	attr(dict, "units") <- time_units
	attr(dict, "raw") <- dict
	return(dict)
}
