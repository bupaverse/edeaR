
#' @export
magrittr::`%>%`


#' @importFrom rlang sym
#'

case_id_ <- function(eventlog) sym(case_id(eventlog))
activity_id_ <- function(eventlog) sym(activity_id(eventlog))
activity_instance_id_ <- function(eventlog) sym(activity_instance_id(eventlog))
resource_id_ <- function(eventlog) sym(resource_id(eventlog))
timestamp_ <- function(eventlog) sym(timestamp(eventlog))
lifecycle_id_ <- function(eventlog) sym(lifecycle_id(eventlog))

construct_call <- function(input_fun, argument) {

	arguments <- map(argument, format_argument)
	arguments <- arguments[!is.na(arguments)]
	if(length(arguments) > 0) {
		arguments <- paste0(names(arguments), " = ", arguments)
		arguments <- paste(arguments, collapse = ",")
		paste0(input_fun,"(",arguments, ")")
	} else {
		paste0(input_fun, "()")
	}

}
construct_call_open <- function(input_fun, argument) {

	arguments <- map(argument, format_argument)
	arguments <- arguments[!is.na(arguments)]
	if(length(arguments) > 0) {
		arguments <- paste0(names(arguments), " = ", arguments)
		arguments <- paste(arguments, collapse = ",")
		paste0(input_fun,",",arguments, ")")
	} else {
		paste0(input_fun, ")")
	}

}

format_argument <- function(arg_list) {
	skip <- FALSE
	if(is.character(arg_list[[1]])) {
		arg_list[[1]] <- paste0("'", arg_list[[1]], "'")
	}
	if(length(arg_list[[1]]) > 1) {
		arg_list[[1]] <- paste0("c(",paste0(arg_list[[1]], collapse = ","), ")")
	}
	if(length(arg_list) > 1) {
		default <- arg_list[[2]]
		current <- arg_list[[1]]
		if(default == current) {
			skip <- TRUE
		}
	}

	if(!skip) {
		arg_list[[1]]
	} else {
		NA
	}
}


construct_input_call <- function(sc, log) {
	if(as.character(sc[[1]])[1] == "%>%") {
		input_cmd <- paste(as.character(sc[[1]])[2], "%>%", str_remove(as.character(sc[[2]][1]), "^i"))
	} else {
		input_cmd <- paste(as.character(log), "%>%", str_remove(as.character(sc[[1]])[1], "^i"))
	}
}





is_attached <- function(x) {
	paste0("package:", x) %in% search()
}

grouped_metric <- function(grouped_eventlog, FUN, ...) {
	# grouped_metric function should be replaced with apply_grouped_fun
	apply_grouped_fun(grouped_eventlog, FUN, ...)
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
		unnest(cols = c(raw)) -> raw

	#unnest result
	temp %>%
		mutate(data = map(data, ~as.data.frame(as.list(.x)))) %>%
		unnest(cols = c(data)) -> output

	# attach raw data
	output <- ungroup(output)
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

return_metric <- function(eventlog, output, level, append, append_column, metric , n_result_col = 2, empty_label = F) {

	append <- maybe_missing(append, default = FALSE)

	if(append && level != "log" && level != "trace") {
		ncol <- ncol(output)
		ids <- 1:(ncol-n_result_col)
		res <- (ncol+1-n_result_col):ncol

		output %>%
			select(ids, !!sym(append_column)) -> output


		if(empty_label) {
			output %>%
				set_names(c(names(.)[ids], paste0(append_column, "_", level))) -> output
		}
		else {
			output %>%
				set_names(c(names(.)[ids], paste0(append_column, "_", level, "_", metric))) -> output
		}
		#	rename_( paste0(metric, "_", level, "_", append_column) == append_column) -> output

		# output %>%
		# 	set_names(c(names(.)[ids], paste0(metric, "_",level,"_",names(.)[res]))) -> output

		suppressMessages(left_join(eventlog, output)) %>%
			re_map(mapping(eventlog)) -> output

		if("grouped_eventlog" %in% class(eventlog)) {
			output <- group_by_at(output, vars(one_of(paste(groups(eventlog)))))
		}
		output


	} else {
		class(output) <- c(str_replace(paste0(level, "_metric"), "-", "_"),metric, class(output)) #replace for resource-activity
		attr(output, "level") <- level
		attr(output, "mapping") <- mapping(eventlog)
		attr(output, "metric_type") <- metric
		if("grouped_eventlog" %in% class(eventlog)) {
			attr(output, "groups") <- groups(eventlog)
		}
		return(output)
	}
}

return_metric_v2 <- function(log,  #original log
							 output,  # output of metric
							 level,  #name of level
							 metric) { #name of metric

	class(output) <- c(str_replace(paste0(level, "_metric"), "-", "_"),metric, class(output)) #replace for resource-activity
	attr(output, "level") <- level
	attr(output, "mapping") <- mapping(log)
	attr(output, "metric_type") <- metric
	if("grouped_eventlog" %in% class(log)) {
		attr(output, "groups") <- groups(log)
	}
	return(output)

}


summary_statistics <- function(vector) {

	vector %>%
		as_tibble() %>%
		summarise("min" = suppressWarnings(min(vector, na.rm = T)),
				  "q1" = quantile(vector, probs = 0.25, na.rm = T),
				  "median" = median(vector, na.rm = T),
				  "mean" = mean(vector, na.rm = T),
				  "q3" = quantile(vector, probs = 0.75, na.rm = T),
				  "max" = max(vector, na.rm = T),
				  "st_dev" = suppressWarnings(sd(vector, na.rm = T)),
				  "iqr" = .data[["q3"]] - .data[["q1"]]) -> s

	return(s)
}

grouped_summary_statistics <- function(data.frame, values, na.rm = T, ...) {
	values <- sym(values)
	data.frame %>%
		summarize(min = suppressWarnings(min(!!values,na.rm = na.rm)),
				  q1 = quantile(!!values, 0.25, na.rm = na.rm),
				  mean = mean(!!values, na.rm = na.rm),
				  median = median(!!values, na.rm = na.rm),
				  q3 = quantile(!!values, 0.75, na.rm = na.rm),
				  max = suppressWarnings(max(!!values, na.rm = na.rm)),
				  st_dev = sd(!!values, na.rm = na.rm),
				  iqr = IQR(!!values, na.rm = na.rm),
				  total = sum(!!values, na.rm = na.rm),
				  ...)
}


check_activities <- function(specified_activities, found_activities, arg = "activities", call = caller_env(), emit_warning = TRUE) {
	wrong <- specified_activities[!(specified_activities %in% found_activities)]
	if (length(wrong) > 0 && emit_warning) {
		cli_warn(c("{.val {length(wrong)}} specified activit{?y/ies} in {.arg {arg}} not found in {.arg log}.",
				   "!" = "Activit{?y/ies} not found: {.val {wrong}}."),
				 call = call)
	}
	specified_activities[!(specified_activities %in% wrong)]
}
col_vector <- function() {
	c("#339999", "#8ADA8A","#FF8749","#6C6DAF","#008FAD", "#A44165",
	  "#005E5E", "#5DBE7C","#A93800", "#AC76C6", "#70D0CF" , "#935592",
	  "#324B4B","#1F884B","#956F5D", "#7E88BC", "#00C9CE","#4A5787",
	  "#428E78", "#007851",  "#7F4C35", "#3681B7", 	  "#009EC0",
	  "#95B1B0",   "#849237",   "#00282A")
}
