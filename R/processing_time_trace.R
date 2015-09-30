

processing_time_trace <- function(eventlog,
								  units = "days") {
	stop_eventlog(eventlog)

	e <- eventlog_wide(eventlog)

	if(!("start" %in% colnames(e)))
		stop("No start events found")
	if(!("complete" %in% colnames(e)))
		stop("No complete events found")

	e$dur <- e$complete - e$start

	e$dur  <- as.double(e$dur , units = units)

	r <- e %>% group_by(case_classifier) %>% summarize(tot_dur = sum(dur))

	ca <- cases_light(eventlog)
	colnames(ca)[colnames(ca)==case_id(eventlog)] <- "case_classifier"

	r <- merge(ca,r)
	r <- select(r, trace, tot_dur)
	r <- r  %>% group_by(trace) %>% summarize(relative_frequency = n(),
															   min = min(tot_dur),
															   q1 = quantile(tot_dur, probs = c(0.25)),
															   median = median(tot_dur),
															   mean = mean(tot_dur),
															   q3 = quantile(tot_dur, probs = c(0.75)),
															   max = max(tot_dur),
															   st_dev = sd(tot_dur),
															   iqr = quantile(tot_dur, probs = c(0.75)) - quantile(tot_dur,probs = c(0.25)))

	r$relative_frequency <- r$relative_frequency / sum(r$relative_frequency)
	r <- r %>% arrange(desc(relative_frequency))

	return(r)

}
