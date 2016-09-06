
size_of_selfloops_trace <- function(eventlog, raw = FALSE) {

	stop_eventlog(eventlog)
	tr <- traces(eventlog)
	sl <- selfloops(eventlog)



	if(raw) {
		suppressMessages(sl %>%
			right_join(tr) %>%
			mutate(length = length - 1) -> tr)

		tr <- arrange(tr, desc(relative_frequency))
		colnames(tr)[colnames(tr)== "relative_frequency"] <- "relative_trace_frequency"
		return(tr)
	}


	suppressMessages(sl %>%
			right_join(tr) %>%
			mutate(length = length - 1) %>%
			group_by(trace, relative_frequency) %>%
			summarize(min = min(length,na.rm = T),
					  q1 = quantile(length, 0.25, na.rm = T),
					  mean = mean(length, na.rm = T),
					  median = median(length, na.rm = T),
					  q3 = quantile(length, 0.75, na.rm =T),
					  max = max(length, na.rm =T),
					  st_dev = sd(length, na.rm = T)) %>%
			mutate(iqr = q3 - q1) -> tr)


	tr <- arrange(tr, desc(relative_frequency))
	colnames(tr)[colnames(tr)=="relative_frequency"] <- "relative_trace_frequency"
	return(tr)


	#legacy

	for(i in 1:nrow(tr)){
		selfloops_lengths <- filter(sl, trace == tr$trace[i])$length
		if(!include_non_selfloops){
			selfloops_lengths <- selfloops_lengths - 1

			if(length(selfloops_lengths) > 0){
				tr$min[i] <- min(selfloops_lengths)
				tr$q1[i] <- quantile(selfloops_lengths, probs = 0.25)
				tr$mean[i] <- mean(selfloops_lengths)
				tr$median[i] <- median(selfloops_lengths)
				tr$q3[i] <- quantile(selfloops_lengths, probs = 0.75)
				tr$max[i] <- max(selfloops_lengths)
				tr$st_dev[i] <- sd(selfloops_lengths)
				tr$iqr <- tr$q3 - tr$q1
			}
			else {
				tr$min[i] <- NA
				tr$q1[i] <- NA
				tr$mean[i] <- NA
				tr$median[i] <- NA
				tr$q3[i] <- NA
				tr$max[i] <- NA
				tr$st_dev[i] <- NA
				tr$iqr[i] <- NA
			}
		}
		else {
			trace_length <- length(strsplit(tr$trace[i], split = ",")[[1]])
			number_of_zero_selfloops <- trace_length - sum(selfloops_lengths)
			selfloops_lengths <- c(selfloops_lengths, rep(1, number_of_zero_selfloops))
			selfloops_lengths <- selfloops_lengths - 1
			tr$min[i] <- min(selfloops_lengths)
			tr$q1[i] <- quantile(selfloops_lengths, probs = 0.25)
			tr$mean[i] <- mean(selfloops_lengths)
			tr$median[i] <- median(selfloops_lengths)
			tr$q3[i] <- quantile(selfloops_lengths, probs = 0.75)
			tr$max[i] <- max(selfloops_lengths)
			tr$st_dev[i] <- sd(selfloops_lengths)
			tr$iqr <- tr$q3 - tr$q1
		}
	}

	tr <- arrange(tr, desc(relative_frequency))
	tr <- select(tr, -absolute_frequency, -trace_id)
	colnames(tr)[colnames(tr)=="relative_frequency"] <- "relative_trace_frequency"
	return(tr)
}
