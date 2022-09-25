idle_time_trace <- function(log, units) {

	cases <- case_list(log)

	log %>%
		idle_time_case(units = units) -> raw

	# Store time units, because dplyr transformations remove the attributes.
	time_units <- attr(raw, "units")

	raw %>%
		merge(cases) %>%
		group_by(trace) %>%
		grouped_summary_statistics("idle_time") -> output

	attr(output, "units") <- time_units
	return(output)
}
