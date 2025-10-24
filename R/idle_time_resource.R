idle_time_resource <- function(log, units) {

	# activate <- NULL
	# active <- NULL
	# next_ts <- NULL
	# dur <- NULL
	# ts <- NULL
	#
	# log %>%
	# 	as.data.frame() %>%
	# 	mutate(activate = ifelse((!!lifecycle_id_(log)) == "start", 1, ifelse((!!lifecycle_id_(log)) == "complete", -1, 0))) %>%
	# 	group_by(!!resource_id_(log)) %>%
	# 	arrange(!!timestamp_(log), .order) %>%
	# 	mutate(active = cumsum(activate),
	# 		   ts = !!timestamp_(log),
	# 		   next_ts = lead(!!timestamp_(log))) %>%
	# 	mutate(dur = difftime(next_ts, ts, units = units)) %>%
	# 	filter(active == 0 & !is.na(dur)) %>%
	# 	summarize(idle_time = sum(dur)) -> output

	dt <- data.table(log)

	LCID <- NULL
	TS <- NULL
	activate <- NULL
	active <- NULL
	next_TS <- NULL
	dur <- NULL

	# Override column names for handling
	setnames(dt,
			 old = c(lifecycle_id(log), resource_id(log), timestamp(log)),
			 new = c("LCID", "RID", "TS"))

	dt[, "activate" := fcase(LCID == "start", 1L,
							 LCID == "complete", -1L,
							 default = 0L)]
	# Order by timestamp and .order per case
	setorderv(dt, cols = c("TS", ".order"))

	dt[,":="("active" = cumsum(activate),
			 "next_TS" = shift(TS, type = "lead")),
		by = "RID"][,
		"dur" := difftime(next_TS, TS, units = units)][
		active == 0L & !is.na(dur)][
		, .("idle_time" = sum(dur)), by = "RID"] -> dt

	# Revert column names to original
	setnames(dt, old = "RID", new = resource_id(log))

	output <- as_tibble(dt)

	attr(output, "units") <- attr(output[["idle_time"]], "units")
	attr(output, "raw") <- output
	return(output)
}
