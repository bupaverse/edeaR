



processing_time_activity_instance <- function(eventlog,
											  units,
											  work_schedule) {
	s <- NULL
	e <- NULL

	eventlog %>%
		group_by(!!activity_instance_id_(eventlog),
				 !!case_id_(eventlog),
				 !!activity_id_(eventlog),
				 !!resource_id_(eventlog)) %>%
		summarize(s = min(!!timestamp_(eventlog)), e = max(!!timestamp_(eventlog))) %>%

		mutate(processing_time = map2_dbl(s, e, compute_processing_time, work_schedule = work_schedule, units = units)) %>%
		ungroup()

}



compute_processing_time <- function(start_dttm, end_dttm, work_schedule, units)  {

	start_time <- NULL
	end_time <- NULL
	start_interval <- NULL
	end_interval <- NULL
	proc_hours <- NULL
	work_hours <- NULL
	difference <- NULL

	if(is.null(work_schedule))
		return(as.double(end_dttm - start_dttm, units = units))
	else {
		start_day <- lubridate::date(x = start_dttm)
		end_day <- lubridate::date(x = end_dttm)

		work_schedule %>%
			filter(!is.na(start_time)) -> work_schedule
		tibble(start_dttm,
			   end_dttm,
			   proc_hours = interval(start_dttm, end_dttm),
			   date = zoo::as.Date(start_day:end_day)) %>%
			mutate(day = ifelse(wday(date) == 1, 7, wday(date)-1)) %>%
			inner_join(work_schedule, by = "day") %>%
			filter(!is.na(start_time)) -> temp

		if(nrow(temp) == 0) {
			return(0)
		}
		 temp %>%
			mutate(start_interval = ymd_hms(paste0(date, start_time))) %>%
			mutate(end_interval = ymd_hms(paste0(date, end_time))) %>%
			mutate(work_hours = interval(start_interval, end_interval)) %>%
			mutate(difference = lubridate::intersect(proc_hours, work_hours)) %>%
			filter(!is.na(difference)) %>%
			summarize(processing_time = sum(as.double(difference, units = "hours"))) %>%
			pull(processing_time)
	}
}


