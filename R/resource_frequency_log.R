
resource_frequency_log <- function(eventlog) {

	resource_classifier <- resource_id(eventlog)
	activity_instance_classifier <- activity_instance_id(eventlog)

	eventlog %>%
		group_by_(resource_classifier, activity_instance_classifier) %>%
		summarize() %>%
		summarize(freq = n()) -> r

	s <- summary(r$freq)
	s <- c(s, St.Dev = sd(r$freq))
	s <- c(s, IQR = s[5] - s[2])
	names(s) <- c("min","q1","median","mean","q3","max","st_dev","iqr")
	s <- data.frame(s)
	s <- as.data.frame(t(s))
	row.names(s) <- NULL
	s <- tbl_df(s)


	return(s)
}
