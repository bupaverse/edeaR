

repeat_selfloops <- function(eventlog) {

	stop_eventlog(eventlog)

	log <- eventlog

	ca <- cases_light(eventlog)

	colnames(log)[colnames(log) == timestamp(eventlog)] <- "timestamp_classifier"
	colnames(log)[colnames(log) == case_id(eventlog)] <- "case_classifier"
	colnames(log)[colnames(log) == activity_id(eventlog)] <- "event_classifier"
	colnames(log)[colnames(log) == activity_instance_id(eventlog)] <- "activity_instance_classifier"
	colnames(log)[colnames(log) == resource_id(eventlog)] <- "resource_classifier"



	log %>%
		group_by(case_classifier, event_classifier, resource, activity_instance_classifier) %>%
		summarize(ts = min(timestamp_classifier)) %>%
		group_by(case_classifier) %>%
		arrange(ts) %>%
		mutate(r = row_number(ts), next_event = lead(event_classifier), next_resource = lead(resource_classifier)) %>%
		filter(event_classifier == next_event, resource_classifier == next_resource) %>% #arrange(case_classifier, ts) %>%
		select(case_classifier, event_classifier, r, ts) %>%
		#mutate(length = 1) %>%
		arrange(ts) %>%
		mutate(next_r = lead(r), diff_r = next_r - r, id = ifelse(ifelse(is.na(diff_r), 0, diff_r == 1),0,1)) %>% arrange(case_classifier, ts) %>%
		#	ungroup() %>%
		mutate(selfloop_group = lag(cumsum(id)),
			   selfloop_group = paste(case_classifier, ifelse(is.na(selfloop_group), 0, selfloop_group), sep= "_")) %>%
		group_by(selfloop_group, event_classifier, resource_classifier,  case_classifier) %>%
		summarize(length = n()) %>%
		ungroup() %>%
		select(case_classifier, event_classifier, resource_classifier, length)  -> t

	colnames(t) <- c(case_id(eventlog), activity_id(eventlog), resource_id(eventlog), "length")

	t %>%
		merge(ca) %>%
		select_("trace", "length", activity_id(eventlog)) %>%
		unique %>%
		mutate(length = length + 1) -> t

	return(t)


	tr <- traces(eventlog)
	output <- data.frame(trace = character(0), length = numeric(0), Activity = character(0) , stringsAsFactors = FALSE)
	r <- 1
	for(i in 1:nrow(tr)) {

		act_seq <- strsplit(tr$trace[i], split = ",")[[1]]
		if(length(act_seq) > 1){
			current_act <- act_seq[1]
			length <- 1
			for(j in 2:length(act_seq)){
				if(current_act == act_seq[j])
					length <- length + 1
				else {
					if(length > 1){
						output <- bind_rows(output, data.frame(t(c(trace = tr$trace[i],length = 9999, Activity = current_act))))
						output$length[r] <- length
						r <- r + 1
					}
					length <- 1
					current_act <- act_seq[j]
				}
			}
			if(length > 1){
				output <- bind_rows(output, data.frame(t(c(trace = tr$trace[i],length = 9999, Activity = current_act))))
				output$length[r] <- length
				r <- r + 1
			}
		}
	}
	output <- tbl_df(output)
	colnames(output)[colnames(output) == "Activity"] <- activity_id(eventlog)
	return(output)
}
