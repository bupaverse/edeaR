
filter_trace_frequency_percentile <- function(eventlog,
								  percentage,
								  reverse){
	cum_sum <- NULL
	lag_cum_sum <- NULL
	.N <- NULL
	absolute <- NULL
	relative <- NULL
	absolute_frequency <- NULL
	relative_frequency <- NULL

	if(nrow(eventlog) == 0) {
		return(filter_case(eventlog, c(), reverse))
	}

	cases <- case_list(eventlog)
	data.table::setDT(cases)

	traces <- cases[, .(absolute_frequency = .N), by = .(trace)]
	traces <- traces[order(absolute_frequency, decreasing = T),
					 relative_frequency:=absolute_frequency/sum(absolute_frequency)]

	traces %>%
		select(trace, absolute = absolute_frequency, relative = relative_frequency) %>%
		arrange(desc(absolute)) %>%
		mutate(cum_sum = cumsum(relative)) %>%
		group_by(absolute) %>%
		mutate(cum_sum = max(cum_sum)) %>%
		ungroup() %>%
		mutate(cum_freq_lag = lag(cum_sum, default = 0))  %>%
		group_by(absolute) %>%
		mutate(cum_freq_lag = min(cum_freq_lag)) %>%
		ungroup() %>%
		filter(cum_freq_lag < percentage) %>%
		merge(cases, by = "trace") %>%
		pull(!!case_id_(eventlog)) -> case_selection


	filter_case(eventlog, case_selection, reverse)
}
