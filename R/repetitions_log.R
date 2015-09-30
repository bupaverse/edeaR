

repetitions_log <- function(eventlog) {

	stop_eventlog(eventlog)

	rep <- repetitions_trace(eventlog)

	colnames(eventlog)[colnames(eventlog)==case_id(eventlog)] <- "case_classifier"
	ncases <- length(unique(eventlog$case_classifier))

	r<- data.frame(c(sum(rep$absolute*rep$relative_trace_frequency)*ncases,
					 sum(rep$absolute*rep$relative_trace_frequency)*ncases/nrow(eventlog)))
	r <- as.data.frame(t(r))
	colnames(r) <- c("absolute","relative")
	row.names(r) <- NULL
	r <- tbl_df(r)
	return(r)

}
