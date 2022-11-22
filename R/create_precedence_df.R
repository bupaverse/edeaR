#' @importFrom data.table as.data.table
#' @importFrom data.table shift
#' @importFrom data.table .SD
#' @importFrom data.table .N

create_precedence_df <- function(log) {

	AID <- NULL
	CID <- NULL
	AIID <- NULL
	TS <- NULL
	start_time <- NULL
	end_time <- NULL
	next_activity <- NULL
	next_start_time <- NULL
	next_end_time <- NULL



	log_dt <- log %>%
		rename(AID = activity_id(log),
			   CID = case_id(log),
			   AIID = activity_instance_id(log),
			   TS = timestamp(log)) %>%
		as.data.table()

	log_dt <- log_dt[, .('start_time' = min(TS),
						 'end_time' = max(TS),
						 'min_order' = min(.order)),
					 by = .(AID, AIID, CID)]

	starts <- log_dt[order(start_time, end_time, min_order), .SD[1] , .(CID)][, c("AID", "end_time", "min_order") := .("Start", start_time, min(min_order) - 1)]
	ends <- log_dt[order(start_time, end_time, min_order), .SD[.N] , .(CID)][, c("AID", "start_time", "min_order") := .("End", end_time, max(min_order) + 1)]

	log_dt <- rbind(starts, log_dt, ends)

	log_dt[order(start_time, end_time, min_order), c("next_activity", "next_start_time","next_end_time") := .(shift(as.character(AID), n = -1),
																											  shift(start_time, n = -1),
																											  shift(end_time, n = -1)), by = .(CID)]

	as_tibble(log_dt)
}
