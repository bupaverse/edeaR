
calculate_work_schedule_times <- function(intervals, work_schedule, units) {
	## CRAN

	day <- NULL
	end_time <- NULL
	start_time <- NULL
	s <- NULL
	e <- NULL
	is_holiday <- NULL
	date_s <- NULL
	date_e <- NULL
	e_sensored <- NULL
	s_sensored <- NULL
	elapsed <- NULL
	prefix <- NULL
	suffix <- NULL
	length_e <- NULL
	length_s <- NULL
	interm_time <- NULL

	## Preprocess work schedule information


	row_id = colnames(intervals)[[1]]


	work_schedule$week_schedule %>%
		mutate(wday = ifelse(day < 7, day + 1, 1))  %>%
		mutate(length = as.double(end_time - start_time, unit = units)) -> sched

	sched %>%
		select(wday_s = wday, end_time, length_s = length) -> start_sched
	sched %>%
		select(wday_e = wday, start_time, length_e = length) -> end_sched

	years <- unique(year(c(intervals$s, intervals$e)))

	holidays <- get_holidays(work_schedule, years)

	## Preprocess interval data

	intervals %>%
		mutate(date_s = as.Date(s),
		   date_e = as.Date(e)) %>%
		mutate(wday_s = wday(s)) %>%
		mutate(wday_e = wday(e)) -> intervals

	## Prepare date dictionary
	tibble(date = seq.Date(min(intervals$date_s), max(intervals$date_e), by = 1)) %>%
		mutate(wday = ifelse(wday(date) == 1, 7 , wday(date) -1)) %>%
		left_join(sched, by = "wday") %>%
		mutate(is_holiday = date %in% holidays) %>%
		filter(!is_holiday, !is.na(length)) -> date_dictionary


	intervals %>%
		filter(date_s == date_e) %>%
		left_join(start_sched, by = "wday_s") %>%
		left_join(end_sched, by = "wday_e") %>%
		mutate(s_sensored = pmax(s, date_s + hours(hour(start_time)) + minutes(minute(start_time)) + seconds(second(start_time)))) %>%
		mutate(e_sensored = pmin(e, date_e + hours(hour(end_time)) + minutes(minute(end_time)) + seconds(second(end_time)))) %>%
		mutate(elapsed = pmax(as.double(e_sensored - s_sensored, units = units), 0)) %>%
		mutate(elapsed = ifelse(is.na(elapsed), 0, elapsed)) %>%
		mutate(elapsed = ifelse(date_s %in% holidays, 0, elapsed)) %>%
		select(row_id, s, e, elapsed) -> same_day


	suppressWarnings(
		{
	intervals %>%
		filter(date_s + 1 == date_e) %>%
		left_join(start_sched, by = "wday_s") %>%
		left_join(end_sched, by = "wday_e") %>%
		mutate(prefix = as.double(date_s + end_time - s, units = units),
			   suffix = as.double(e  - date_e - start_time, units = units)) %>%
		mutate(prefix = pmax(pmin(prefix, length_s), 0),
			   suffix = pmax(pmin(suffix, length_e), 0)) %>%
		mutate(prefix = ifelse(is.na(prefix), 0, prefix),
			   suffix = ifelse(is.na(suffix), 0, suffix)) %>%
		mutate(prefix = ifelse(date_s %in% holidays, 0, prefix)) %>%
		mutate(suffix = ifelse(date_e %in% holidays, 0, suffix)) %>%
		mutate(elapsed = prefix + suffix) %>%
		select(row_id, s, e, elapsed) -> consecutive_day

	intervals %>%
		filter(date_s + 1 < date_e) %>%
		left_join(start_sched, by = "wday_s") %>%
		left_join(end_sched, by = "wday_e") %>%
		mutate(prefix = as.double(date_s + end_time - s, units = units),
			   suffix = as.double(e  - date_e - start_time, units = units)) %>%
		mutate(prefix = pmax(pmin(prefix, length_s), 0),
			   suffix = pmax(pmin(suffix, length_e), 0)) %>%
		mutate(prefix = ifelse(is.na(prefix), 0, prefix),
			   suffix = ifelse(is.na(suffix), 0, suffix)) %>%
		mutate(interm_time = map2_dbl(date_s, date_e, get_intermediate_time, date_dictionary))  %>%
		mutate(prefix = ifelse(date_s %in% holidays, 0, prefix)) %>%
		mutate(suffix = ifelse(date_e %in% holidays, 0, suffix)) %>%
		mutate(elapsed = prefix + suffix + interm_time)  %>%
		select(row_id, s, e, elapsed) -> multiple_days
		})
	bind_rows(list(same_day, consecutive_day, multiple_days))

}

get_intermediate_time <- function(from, to, date_dictionary) {
	date_dictionary %>%
		filter(date < to, date > from) %>%
		pull(length) %>%
		sum(na.rm = T)
}
