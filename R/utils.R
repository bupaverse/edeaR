
#' @export
magrittr::`%>%`


deprecated_level <- function(level,...) {
	l <- list(...)
	if(length(l) > 0 && any(stringr::str_detect("level_of_analysis",names(l)))) {
		warning("Argument level_of_analysis is deprecated. Use level instead.")
		l[stringr::str_detect("level_of_analysis",names(l))][[1]]
	} else {
		level
	}
}

deprecated_perc <- function(perc,...) {
	l <- list(...)
	if(length(l) > 0 && any(stringr::str_detect("percentile_cut_off",names(l)))) {
		warning("Argument percentile_cut_off is deprecated. Use percentage instead.")
		l[stringr::str_detect("percentile_cut_off",names(l))][[1]]
	} else {
		perc
	}
}

deprecated_lower_thr <- function(int_1,...) {
	l <- list(...)
	if(length(l) > 0 && any(stringr::str_detect("lower_threshold",names(l)))) {
		warning("Arguments lower_threshold and upper_threshold are deprecated. Use interval instead.")
		l[stringr::str_detect("lower_threshold",names(l))][[1]]
	} else {
		int_1
	}
}

deprecated_upper_thr <- function(int_2,...) {
	l <- list(...)
	if(length(l) > 0 && any(stringr::str_detect("upper_threshold",names(l)))) {
		warning("Arguments lower_threshold and upper_threshold are deprecated. Use interval instead.")
		l[stringr::str_detect("upper_threshold",names(l))][[1]]
	} else {
		int_2
	}
}

deprecated_starting_point <- function(s, ...) {
	l <- list(...)
	if(length(l) > 0 && any(stringr::str_detect("start_point",names(l)))) {
		warning("Arguments start_point and end_point are deprecated. Use interval instead.")
		l[stringr::str_detect("start_point",names(l))][[1]]
	} else {
		s
	}
}

deprecated_end_point <- function(e, ...) {
	l <- list(...)
	if(length(l) > 0 && any(stringr::str_detect("end_point",names(l)))) {
		warning("Arguments start_point and end_point are deprecated. Use interval instead.")
		l[stringr::str_detect("end_point",names(l))][[1]]
	} else {
		e
	}
}


is_attached <- function(x) {
	paste0("package:", x) %in% search()
}

grouped_metric <- function(grouped_eventlog, FUN, ...) {
	mapping <- mapping(grouped_eventlog)

	grouped_eventlog %>%
		nest %>%
		mutate(data = map(data, re_map, mapping)) %>%
		mutate(data = map(data, FUN, ...)) %>%
		unnest -> output
	attr(output, "groups") <- groups(grouped_eventlog)
	return(output)
}

grouped_metric_raw_log <- function(grouped_eventlog, FUN, ...) {

	## Apply function on a grouped eventlog at the log-level


	mapping <- mapping(grouped_eventlog)

	#apply function for each group
	grouped_eventlog %>%
		nest %>%
		mutate(data = map(data, re_map, mapping)) %>%
		mutate(data = map(data, FUN, ...)) -> temp

	#select the raw data for each group

	temp %>%
		mutate(raw = map(data, attr, "raw")) %>%
		select(-data) %>%
		unnest() -> raw

	#unnest result
	temp %>%
		mutate(data = map(data, ~as.data.frame(as.list(.x)))) %>%
		unnest() -> output

	# attach raw data
	attr(output, "raw") <- raw
	attr(output, "groups") <- groups(grouped_eventlog)

	return(output)
}

grouped_filter <- function(eventlog, FILTER, ...) {
	mapping <- mapping(eventlog)

	eventlog %>%
		nest %>%
		mutate(data = map(data, re_map, mapping)) %>%
		mutate(data = map(data, FILTER, ...)) %>%
		unnest %>%
		re_map(mapping) %>%
		group_by_at(vars(one_of(paste(groups(eventlog))))) -> output

	output
}

return_metric <- function(eventlog, output, level, append, metric, n_result_col = 2) {

	if(append && level != "log" && level != "trace") {
		ncol <- ncol(output)
		ids <- 1:(ncol-n_result_col)
		res <- (ncol+1-n_result_col):ncol

		output %>%
			set_names(c(names(.)[ids], paste0(metric, "_",level,"_",names(.)[res]))) -> output
		suppressMessages(left_join(eventlog, output)) %>%
			re_map(mapping(eventlog)) -> output

		if("grouped_eventlog" %in% class(eventlog)) {
			output <- group_by_at(output, vars(one_of(paste(groups(eventlog)))))
		}
		output


	} else {
		class(output) <- c(paste0(level, "_metric"),metric, class(output))
		attr(output, "level") <- level
		attr(output, "mapping") <- mapping(eventlog)
		return(output)
	}
}


cases_light <- function(eventlog){
	if(!("eventlog" %in% class(eventlog)))
		stop("Function only applicable for eventlog object")

	eDT <- data.table::as.data.table(eventlog)
	cases <- eDT[,
				 list("timestamp_classifier" = min(get(timestamp(eventlog)))),
				 by = list("A" = get(case_id(eventlog)), "B" = get(activity_instance_id(eventlog)), "C" = get(activity_id(eventlog)))]
	cases <- cases[order(get("timestamp_classifier"), get("C")),
				   list(trace = paste(get("C"), collapse = ",")),
				   by = list("CASE" = get("A"))]
	cases <- cases %>% mutate(trace_id = as.numeric(factor(!!as.symbol("trace")))) %>%
		rename(!!as.symbol(case_id(eventlog)) := "CASE")
	#	cases <- eventlog %>%
	#		group_by(case_classifier, activity_instance_classifier, event_classifier) %>%
	#		summarize(timestamp_classifier = min(timestamp_classifier)) %>%
	#		group_by(case_classifier) %>%
	#		arrange(timestamp_classifier) %>%
	#		summarize(trace = paste(event_classifier, collapse = ",")) %>%
	#		mutate(trace_id = as.numeric(factor(trace)))

	casesDT <- data.table(cases)
	cases <- cases %>% data.frame

	#traces <- cases %>%
	#	group_by(trace, trace_id) %>%
	#	summarize(absolute_frequency = n()) %>%
	#	ungroup() %>%
	#	arrange(desc(absolute_frequency)) %>%
	#	mutate(relative_frequency = absolute_frequency/sum(absolute_frequency))


	return(cases)

}


traces_light <- function(eventlog){



	eDT <- data.table::data.table(eventlog)

	cases <- eDT[,
				 list("timestamp_classifier" = min(get(timestamp(eventlog)))),
				 by = list("A" = get(case_id(eventlog)), "B" = get(activity_instance_id(eventlog)), "C" = get(activity_id(eventlog)))]
	cases <- cases[order(get("timestamp_classifier"), get("C")),
				   list(trace = paste(get("C"), collapse = ",")),
				   by = list("CASE" = get("A"))]
	cases <- cases %>% mutate(trace_id = as.numeric(factor(!!as.symbol("trace")))) %>%
		rename(!!as.symbol(case_id(eventlog)) := "CASE")

	#	cases <- eventlog %>%
	#		group_by(case_classifier, activity_instance_classifier, event_classifier) %>%
	#		summarize(timestamp_classifier = min(timestamp_classifier)) %>%
	#		group_by(case_classifier) %>%
	#		arrange(timestamp_classifier) %>%
	#		summarize(trace = paste(event_classifier, collapse = ",")) %>%
	#		mutate(trace_id = as.numeric(factor(trace)))


	.N <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL

	casesDT <- data.table(cases)

	traces <- casesDT[, .(absolute_frequency = .N), by = .(trace)]

	traces <- traces[order(absolute_frequency, decreasing = T),relative_frequency:=absolute_frequency/sum(absolute_frequency)]
	traces <- tbl_df(traces)

	#traces <- eventlog %>%
	#	group_by(case_classifier, activity_instance_classifier, event_classifier) %>%
	#	summarize(timestamp_classifier = min(timestamp_classifier)) %>%
	#	group_by(case_classifier) %>%
	#	arrange(timestamp_classifier) %>%
	#	summarize(trace = paste(event_classifier, collapse = ",")) %>%
	#	group_by(trace) %>%
	#	summarize()

	return(traces)

}

summary_statistics <- function(vector) {


	s <- summary(vector)
	s <- c(s, St.Dev = sd(vector))
	s <- c(s, IQR = s[5] - s[2])
	names(s) <- c("min","q1","median","mean","q3","max","st_dev","iqr")
	return(s)
}

stop_eventlog <- function(eventlog)
	if(!("eventlog" %in% class(eventlog)))
		stop("Function only applicable for class eventlog")
