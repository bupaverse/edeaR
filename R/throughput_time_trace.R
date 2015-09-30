

throughput_time_trace <- function(eventlog,
								  units = "days") {
	stop_eventlog(eventlog)

	csum <- cases(eventlog)

	ncases <- nrow(csum)

	s <- csum %>% group_by(trace_id, trace) %>% summarize(relative_trace_frequency = n(),
														  min = min(duration_in_days),
														  q1 = quantile(duration_in_days, probs = c(0.25)),
														  median = median(duration_in_days),
														  mean = mean(duration_in_days),
														  q3 = quantile(duration_in_days, probs = c(0.75)),
														  max = max(duration_in_days),
														  st_dev = sd(duration_in_days),
														  iqr = quantile(duration_in_days, probs = c(0.75)) - quantile(duration_in_days, probs = c(0.25))) %>% ungroup()

	s <-  s  %>% arrange(desc(relative_trace_frequency)) %>% select( -trace_id)

	s$relative_trace_frequency <- s$relative_trace_frequency/ncases
	return(s)


}
