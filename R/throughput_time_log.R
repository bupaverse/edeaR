


throughput_time_log <- function(eventlog,
								 units = "days") {
	stop_eventlog(eventlog)

	csum <- durations(eventlog,
					  units = units)

	colnames(csum)[colnames(csum) == paste("duration_in_", units, sep = "")] <- "duration_classifier"

	csum$duration <- as.double(csum$duration_classifier, units = units)

	s <- summary(csum$duration)
	s <- c(s, St.Dev = sd(csum$duration))
	s <- c(s, IQR = s[5] - s[2])
	names(s) <- c("min","q1","median","mean","q3","max","st_dev","iqr")

	s <- as.data.frame(s)
	s <- t(s)
	row.names(s) <- NULL
	return(s)
}
