


trace_length_log <- function(eventlog) {

	stop_eventlog(eventlog)

	csum <- trace_length_case(eventlog)

	s <- summary(csum$trace_length)
	s <- c(s, St.Dev = sd(csum$trace_length))
	s <- c(s, IQR = s[5] - s[2])
	names(s) <- c("min","q1","median","mean","q3","max","st_dev","iqr")
	s <- as.data.frame(s)
	s <- t(s)
	row.names(s) <- NULL

	return(s)

}
