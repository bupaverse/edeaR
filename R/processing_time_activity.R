
processing_time_activity <- function(eventlog,
							  units = "days") {
	stop_eventlog(eventlog)

	e <- eventlog_wide(eventlog)

	if(!("start" %in% colnames(e)))
		stop("No start events found")
	if(!("complete" %in% colnames(e)))
		stop("No complete events found")

	e$dur <- e$complete - e$start

	e$dur  <- as.double(	e$dur , units = units)

	r <- e %>% group_by(event_classifier) %>% summarize(relative_frequency = n(),
															   min = min(dur),
															   q1 = quantile(dur, probs = c(0.25)),
															   median = median(dur),
															   mean = mean(dur),
															   q3 = quantile(dur, probs = c(0.75)),
															   max = max(dur),
															   st_dev = sd(dur),
															   iqr = quantile(dur, probs = c(0.75)) - quantile(dur,probs = c(0.25))
	)


	r <- r %>% arrange(desc(relative_frequency))

	colnames(r)[colnames(r)=="event_classifier"] <- activity_id(eventlog)

	return(r)

}
