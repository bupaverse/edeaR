idle_time_case <- function(eventlog, units) {

	activate <- NULL
	active <- NULL
	ts <- NULL
	next_ts <- NULL
	dur <- NULL

	eventlog %>%
		mutate(activate = ifelse((!!lifecycle_id_(eventlog)) == "start", 1, ifelse((!!lifecycle_id_(eventlog)) == "complete", -1, 0))) %>%
		group_by_case %>%
		arrange(!!timestamp_(eventlog), .order) %>%
		mutate(active = cumsum(activate),
			   ts = !!timestamp_(eventlog),
			   next_ts = lead(!!timestamp_(eventlog))) %>%
		mutate(dur = as.double(next_ts - ts, units = units)) %>%
		filter(active == 0 & !is.na(dur)) %>%
		summarize(idle_time = sum(dur))
}
