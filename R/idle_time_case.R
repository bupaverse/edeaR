idle_time_case <- function(log, units) {

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


	# Override column names for handling
	setnames(dt,
			 old = c(lifecycle_id(log), case_id(log), timestamp(log)),
			 new = c("LCID", "CID", "TS"))

	dt[, "activate" := fcase(LCID == "start", 1L,
	                         LCID == "complete", -1L,
                             default = 0L)]
    # Order by timestamp and .order per case
    setorderv(dt, cols = c("TS", ".order"))

    dt[,":="("active" = cumsum(activate),
             "next_TS" = shift(TS, type = "lead")),
        by = "CID"][,
        "dur" := difftime(next_TS, TS, units = units)][
	    active == 0L & !is.na(dur)][
	   , .("idle_time" = sum(dur)), by = "CID"] -> dt

	# Revert column names to original
	setnames(dt, old = "CID", new = case_id(log))

	output <- as_tibble(dt)

	attr(output, "units") <- attr(output[["idle_time"]], "units")
	attr(output, "raw") <- output

	return(output)
}
