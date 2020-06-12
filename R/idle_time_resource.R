idle_time_resource <- function(eventlog, units) {

	activate <- NULL
	active <- NULL
	next_ts <- NULL
	dur <- NULL
	ts <- NULL

	eventlog %>%
		as.data.frame() %>%
		mutate(activate = ifelse((!!lifecycle_id_(eventlog)) == "start", 1, ifelse((!!lifecycle_id_(eventlog)) == "complete", -1, 0))) %>%
		group_by(!!resource_id_(eventlog)) %>%
		arrange(!!timestamp_(eventlog), .order) %>%
		mutate(active = cumsum(activate),
			   ts = !!timestamp_(eventlog),
			   next_ts = lead(!!timestamp_(eventlog))) %>%
		mutate(dur = as.double(next_ts - ts, units = units)) %>%
		filter(active == 0 & !is.na(dur)) %>%
		summarize(idle_time = sum(dur))


}
