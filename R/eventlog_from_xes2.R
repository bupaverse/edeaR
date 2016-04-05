#' @title eventlog_from_xes
#' @description Extracts eventlog from a xes-file.
#' @param xesfile Reference to a .xes file, conforming to the xes-standard.
#' @seealso \url{http://www.xes-standard.org/}
#' @export eventlog_from_xes2


eventlog_from_xes2 <- function(xesfile = file.choose()) {

x <- read_xml(xesfile)

log <-  xml_children(x)


traces <- log[xml_name(log) == "trace"]

message(paste0("Importing log with ", length(traces), " cases..."))


children_traces <- traces %>% xml_children
children_traces_types <- children_traces %>% xml_name
children_traces_attr <- children_traces %>% xml_attrs
for(i in 1:length(children_traces_attr))
	if(children_traces_attr[[i]] %>% length == 0)
		children_traces_attr[[i]] <- c(key = NA, value = NA)

children_traces_attr %>% unlist %>%
	matrix(ncol = 2, byrow = T) %>%
	as.data.frame %>%
	mutate(t = ifelse(V1 != "concept:name" | is.na(V1),0,1), case_id = cumsum(t)) -> children_traces_attr

trace_childrens <- data.table(caseids = children_traces_attr$case_id, children_traces)

trace_childrens$element_type <- sapply(trace_childrens[,children_traces], xml_name)

trace_events <- trace_childrens[element_type == "event",]

events_per_case <- lapply(trace_events[,children_traces], xml_children)

n_attributes_per_trace <- events_per_case %>% lapply(length) %>% unlist()
attributes <- events_per_case %>% sapply(xml_attrs) %>% unlist %>% matrix(ncol = 2, byrow = T)
eventids <- n_attributes_per_trace %>% mapply(rep, 1:nrow(trace_events), each = ., SIMPLIFY = F) %>% unlist()
event_attributes_long <- data.table(event_id = eventids,
									case_id = n_attributes_per_trace %>% rep(trace_events$caseids, .),
									keyid = attributes[,1], value = attributes[,2])

message("Finishing...")
#event_attributes_long <- suppressWarnings(bind_rows(event_attributes_list))

case_ids <- children_traces_attr %>%
	rename(case_concept.name = V2) %>%
	filter(V1 == "concept:name") %>%
	select(2,4)


event_attributes_output <- spread(event_attributes_long, "keyid", "value") %>%
	tbl_df %>%
	inner_join(case_ids) %>%
	rename(activity_instance = event_id) %>%
	select(case_concept.name, everything(), -case_id)

colnames(event_attributes_output)[2:ncol(event_attributes_output)] <- gsub(":", "\\.",paste0("event_",colnames(event_attributes_output)[2:ncol(event_attributes_output)]))

event_attributes_output %>%
	eventlog(case_id = "case_concept.name",
			 activity_id = "event_concept.name",
			 activity_instance_id = "event_activity_instance",
			 lifecycle_id = "event_lifecycle.transition",
			 timestamp = "event_time.timestamp") %>%
	return

}
