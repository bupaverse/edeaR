

start_activities_log <- function(eventlog) {

	r <- start_activities_activity(eventlog)
	r<- data.frame(c(nrow(r),nrow(r)/n_activities(eventlog)))
	r <- as.data.frame(t(r))
	colnames(r) <- c("absolute","relative")
	row.names(r) <- NULL
	r <- tbl_df(r)
	return(r)
}
