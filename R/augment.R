#' @title Augment Log
#'
#' @description Augment log with results from metric computation.
#'
#' @param metric Metric computed by edeaR
#' @param log \code{\link[bupaR]{log}}: Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}},
#' \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.) that was used to compute the \code{metric}.
#' @param columns \code{\link{character}} vector: Column names from the \code{metric} that you want to add to the \code{log}. If missing, defaults to all columns.
#' @param prefix \code{\link{character}}: Prefix to be added to the newly added metric columns in the \code{log}.
#'
#' @concept metrics_other
#'
#' @examples
#' \dontrun{
#' sepsis %>%
#' 	throughput_time("case") %>%
#' 	augment(sepsis)
#' }
#'
#' @return Object of class \code{\link[bupaR]{log}} or derivatives (\code{\link[bupaR]{grouped_log}},
#' \code{\link[bupaR]{eventlog}}, \code{\link[bupaR]{activitylog}}, etc.). Same class as the \code{log} input.
#' @export
augment <- function(metric, log, columns, prefix = "") {
	UseMethod("augment")
}

#' @describeIn augment Augment log metric
#' @export

augment.log_metric <- function(metric, log, columns, prefix = "") {

	if(!missing(columns)) {
		metric <- metric %>% select(columns, as.character(attr(metric, "groups")))
	}

	if(!is.null(prefix) && prefix != "") {
		metric <- metric %>% rename_with(~paste(prefix, ., sep = "_"), -as.character(attr(metric, "groups")))
	}

	if(is.null(attr(metric, "groups"))) {
		# quite useless augment in this case, but might be relevant in a purrr context.
		bind_cols(log, metric)
	} else {
		full_join(log, metric, by = as.character(attr(metric, "groups")))
	}
}

#' @describeIn augment Augment case metric
#' @export

augment.case_metric <-  function(metric, log, columns, prefix = "") {

	if(!missing(columns)) {
		metric <- metric %>% select(case_id(log), columns, as.character(attr(metric, "groups")))
	}

	if(!is.null(prefix) && prefix != "") {
		metric <- metric %>% rename_with(~paste(prefix, ., sep = "_"), c(-case_id(log), -as.character(attr(metric, "groups"))))
	}

	if(is.null(attr(metric, "groups"))) {
		full_join(log, metric, by = case_id(log))
	} else {
		full_join(log, metric, by = c(case_id(log), as.character(attr(metric, "groups"))))
	}
}

#' @describeIn augment Augment activity metric
#' @export

augment.activity_metric <-  function(metric, log, columns, prefix = "") {

	if(!missing(columns)) {
		metric <- metric %>% select(activity_id(log), columns, as.character(attr(metric, "groups")))
	}

	if(!is.null(prefix) && prefix != "") {
		metric <- metric %>% rename_with(~paste(prefix, ., sep = "_"), c(-activity_id(log), -as.character(attr(metric, "groups"))))
	}

	if(is.null(attr(metric, "groups"))) {
		full_join(log, metric, by = activity_id(log))
	} else {
		full_join(log, metric, by = c(activity_id(log), as.character(attr(metric, "groups"))))
	}
}

#' @describeIn augment Augment resource metric
#' @export

augment.resource_metric <-  function(metric, log, columns, prefix = "") {

	if(!missing(columns)) {
		metric <- metric %>% select(resource_id(log), columns, as.character(attr(metric, "groups")))
	}

	if(!is.null(prefix) && prefix != "") {
		metric <- metric %>% rename_with(~paste(prefix, ., sep = "_"), c(-resource_id(log), -as.character(attr(metric, "groups"))))
	}

	if(is.null(attr(metric, "groups"))) {
		full_join(log, metric, by = resource_id(log))
	} else {
		full_join(log, metric, by = c(resource_id(log), as.character(attr(metric, "groups"))))
	}
}


#' @describeIn augment Augment resource-activity metric
#' @export

augment.resource_activity_metric <-  function(metric, log, columns, prefix = "") {

	if(!missing(columns)) {
		metric <- metric %>% select(resource_id(log), activity_id(log), columns, as.character(attr(metric, "groups")))
	}

	if(!is.null(prefix) && prefix != "") {
		metric <- metric %>% rename_with(~paste(prefix, ., sep = "_"), c(-resource_id(log), -activity_id(log), -as.character(attr(metric, "groups"))))
	}

	if(is.null(attr(metric, "groups"))) {
		full_join(log, metric, by = c(resource_id(log), activity_id(log)))
	} else {
		full_join(log, metric, by = c(resource_id(log), activity_id(log), as.character(attr(metric, "groups"))))
	}
}

#' @describeIn augment Augment trace metric
#' @export

augment.trace_metric <-  function(metric, log, columns, prefix = "") {


	if(!is.null(attr(metric, "raw"))) { #raw data with trace + case mapping is present, huraay
		join_table <- attr(metric, "raw") %>%
			select(case_id(log), trace)
	} else { #we have to compute them #sad
		join_table <- case_list(log) %>% select(trace, case_id(log))
	}



	if(!missing(columns)) {
		metric <- metric %>% select(trace, columns, as.character(attr(metric, "groups")))
	}

	if(!is.null(prefix) && prefix != "") {
		metric <- metric %>% rename_with(~paste(prefix, ., sep = "_"), -trace, -as.character(attr(metric, "groups")))
	}

	metric %>%
		full_join(join_table, by = "trace") %>%
		select(-trace) -> metric


	if(is.null(attr(metric, "groups"))) {
		full_join(log, metric, by = case_id(log))
	} else {
		full_join(log, metric, by = c(case_id(log), as.character(attr(metric, "groups"))))
	}
}

