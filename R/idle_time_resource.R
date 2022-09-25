idle_time_resource <- function(log, units) {

	activate <- NULL
	active <- NULL
	next_ts <- NULL
	dur <- NULL
	ts <- NULL

	log %>%
		as.data.frame() %>%
		mutate(activate = ifelse((!!lifecycle_id_(log)) == "start", 1, ifelse((!!lifecycle_id_(log)) == "complete", -1, 0))) %>%
		group_by(!!resource_id_(log)) %>%
		arrange(!!timestamp_(log), .order) %>%
		mutate(active = cumsum(activate),
			   ts = !!timestamp_(log),
			   next_ts = lead(!!timestamp_(log))) %>%
		mutate(dur = difftime(next_ts, ts, units = units)) %>%
		filter(active == 0 & !is.na(dur)) %>%
		summarize(idle_time = sum(dur)) -> output

	attr(output, "units") <- attr(output[["idle_time"]], "units")
	return(output)
}
