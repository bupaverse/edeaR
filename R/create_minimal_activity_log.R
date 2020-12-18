
create_minimal_activity_log <- function(eventlog) {
	eDT <- data.table::data.table(eventlog)

	data.table::setorderv(eDT, cols = c(case_id(eventlog), timestamp(eventlog), ".order"))
	dplyr::tbl_df(unique(eDT, by = c(case_id(eventlog), activity_instance_id(eventlog), activity_id(eventlog))))
}

