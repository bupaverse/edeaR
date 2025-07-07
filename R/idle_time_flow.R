idle_time_flow <- function(log, units) {

	# activate <- NULL
	# active <- NULL
	# ts <- NULL
	# next_ts <- NULL
	# dur <- NULL

	# patients %>%
	# 	mutate(activate = ifelse((!!lifecycle_id_(log)) == "start", 1, ifelse((!!lifecycle_id_(log)) == "complete", -1, 0))) %>%
	# 	group_by_case() %>%
	# 	arrange(!!timestamp_(log), .order) %>%
	# 	mutate(active = cumsum(activate),
	# 		   ts = !!timestamp_(log),
	# 		   next_ts = lead(!!timestamp_(log))) %>%
	# 	mutate(dur = difftime(next_ts, ts, units = units)) %>%
	# 	filter(active == 0 & !is.na(dur)) %>%
	# 	summarize(idle_time = sum(dur)) -> output

	dt <- data.table(log)

	LCID <- NULL
	activate <- NULL
	TS <- NULL
	active <- NULL
	next_TS <- NULL
	dur <- NULL
	CID <- NULL
	AID <- NULL
	AIID <- NULL
	start <- NULL


	# Override column names for handling
	setnames(dt,
			 old = c(lifecycle_id(log), case_id(log), timestamp(log), activity_id(log), activity_instance_id(log)),
			 new = c("LCID", "CID", "TS", "AID", "AIID"))

	dt[, .("start" = min(TS), "complete" = max(TS)), by = .(CID,AID, AIID)] -> dt

	dt <- gather(dt, LCID, TS, start, complete ) %>% data.table()

	dt[, "activate" := fcase(LCID == "start", 1L,
							 LCID == "complete", -1L,
							 default = 0L)]
	# Order by timestamp and .order per case
	setorderv(dt, cols = c("TS"))

	dt[,":="("active" = cumsum(activate),
			 "next_TS" = shift(TS, type = "lead"),
			 "next_ACT" = shift(AID, type = "lead")),
	   by = "CID"][,
	   			"dur" := difftime(next_TS, TS, units = units)][
	   				active == 0L & !is.na(dur)] -> dt

	# Revert column names to original
	setnames(dt, old = c("CID","AID", "next_ACT", "dur", "AIID"), new = c(case_id(log), "from", "to", "idle_time", activity_instance_id(log)))

	output <- as_tibble(dt) %>%
		select(-LCID, -TS, -next_TS, -active, -activate)

	attr(output, "units") <- attr(output[["idle_time"]], "units")
	return(output)
}
