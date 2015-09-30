

number_of_selfloops_log <- function(eventlog) {

	stop_eventlog(eventlog)

	res <- number_of_selfloops_trace(eventlog)

	r <- summary(res$absolute)
	r <- c(r, St.Dev = sd(res$absolute))
	r <- c(r, IQR = r[5] - r[2])
	names(r) <- c("min","q1","median","mean","q3","max","st_dev","iqr")
	r <- data.frame(r)
	r <- as.data.frame(t(r))
	row.names(r) <- NULL
	r <- tbl_df(r)
	return(r)

}
