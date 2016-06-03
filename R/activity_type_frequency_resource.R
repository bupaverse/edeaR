
activity_type_frequency_resource <- function(eventlog) {

	stop_eventlog(eventlog)

	resource_classifier <- resource_id(eventlog)
	case_classifier <- case_id(eventlog)
	colnames(eventlog)[colnames(eventlog) == activity_instance_id(eventlog)] <- "activity_instance_classifier"

	r <- eventlog %>%
		group_by_(resource_classifier, case_classifier) %>%
		summarize(Freq = n_distinct(activity_instance_classifier)) %>%
		summarize(min = min(Freq),
				   q1 =quantile(Freq, probs=0.25),
				   mean = mean(Freq),
				   median = median(Freq) * 1.0,
				   q3 =quantile(Freq, probs=0.75),
				   max = max(Freq),
				   st_dev = sd(Freq),
				   iqr = quantile(Freq, probs=0.75) - quantile(Freq, probs=0.25),
				   tot = sum(Freq))

	return(r)
}
