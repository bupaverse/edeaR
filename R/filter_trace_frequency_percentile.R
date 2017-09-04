
filter_trace_frequency_percentile <- function(eventlog,
								  percentile_cut_off,
								  reverse = F){




	if(reverse == F)
		case_selection <- merge(cases_light(eventlog),
								trace_coverage(eventlog, "trace") %>%
									mutate(lag_cum_sum = lag(cum_sum, default = 0)) %>%
									filter(lag_cum_sum <= percentile_cut_off))
	else
		case_selection <- merge(cases_light(eventlog),
								trace_coverage(eventlog, "trace") %>%
									mutate(lag_cum_sum = lag(cum_sum, default = 0)) %>%
									filter(lag_cum_sum > percentile_cut_off))



	colnames(eventlog)[colnames(eventlog)==case_id(eventlog)] <- "case_classifier"
	colnames(case_selection)[colnames(case_selection) == case_id(eventlog)] <- "case_classifier"


	output <- filter(eventlog,  case_classifier %in% case_selection$case_classifier)
	colnames(output)[colnames(output) == "case_classifier"] <- case_id(eventlog)

	output <- re_map(output, mapping(eventlog))


	return(output)

}
