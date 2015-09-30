
activity_type_frequency_activity <- function(eventlog) {

	stop_eventlog(eventlog)

	r <- activities(eventlog)
	colnames(r)[2] <- "absolute"
	colnames(r)[3] <- "relative"
	r$cum_sum <- cumsum(r$relative)

	return(r)
}
