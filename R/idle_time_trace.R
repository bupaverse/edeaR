idle_time_trace <- function(eventlog, units) {

	cases <- cases_light(eventlog)
	idle_times <- idle_time_case(eventlog, units)

	merge(idle_times, cases) %>%
		group_by(trace) %>%
		summarize(min = min(idle_time,na.rm = T),
				  q1 = quantile(idle_time, 0.25, na.rm = T),
				  mean = mean(idle_time, na.rm = T),
				  median = median(idle_time, na.rm = T),
				  q3 = quantile(idle_time, 0.75, na.rm =T),
				  max = max(idle_time, na.rm =T),
				  st_dev = sd(idle_time, na.rm = T)) %>%
		return()
}
