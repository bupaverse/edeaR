

#' Create work schedule
#'
#' @param start_time Character indicating the usual start time for workdays (hh:mm:ss)
#' @param end_time Character indicating the usual end time for workdays (hh:mm:ss)
#'
#' @concept work_schedule
#'
#' @export
#'
create_work_schedule <- function(start_time = "9:00:00", end_time = "17:00:00") {

	name <- NULL



	tibble(day = 1:7,
		   start_time = parse_hms(c(rep(start_time, 5), NA, NA)),
		   end_time = parse_hms(c(rep(end_time, 5), NA, NA))
	) -> week_schedule




	fixed_holidays <- tribble(~month, ~day, ~name,
							  1, 1, "New Year's Day",
							  12, 25, "Christmas")


	floating_holidays <- tribble(~date, ~name) %>%
		mutate(date = as.Date(date),
			   name = as.character(name))

	holiday_periods <- tribble(~from, ~to) %>%
		mutate_all(as.Date)

	work_schedule <- list(week_schedule = week_schedule,
						  fixed_holidays = fixed_holidays,
						  floating_holidays = floating_holidays,
						  holiday_periods = holiday_periods)



	class(work_schedule) <- c("work_schedule", class(work_schedule))

	return(work_schedule)
}

#' Adjust days in work schedule
#'
#' @param work_schedule Work schedule created with create_work_schedule
#' @param day A numeric vector containing the days to be changed. 1 = monday.
#' @param start_time The new start time for selected days (hh:mm:ss)
#' @param end_time The new end time for selected days (hh:mm:ss)
#'
#' @concept work_schedule


#' @export
#'
change_day <- function(work_schedule, day, start_time, end_time) {
	if(!("work_schedule") %in% class(work_schedule)) {
		stop("work_schedule should be created with function create_work_schedule")
	}
	week <- work_schedule$week_schedule

	week$start_time[week$day %in% day] <- parse_hms(start_time)
	week$end_time[week$day %in% day] <- parse_hms(end_time)

	work_schedule$week_schedule <- week
	return(work_schedule)
}
#' Add fixed holiday to work schedule
#' @param work_schedule Work schedule created with create_work_schedule
#' @param name Name of holiday
#' @param month Month in which fixed holiday takes place
#' @param day Day of fixed holiday
#'
#' @concept work_schedule
#'
#' @export
add_fixed_holiday <- function(work_schedule, name, month, day) {
	work_schedule$fixed_holidays <- work_schedule$fixed_holidays %>%
		bind_rows(tibble(month = month, day = day, name = name))
	return(work_schedule)
}
#' Add floating holiday to work schedule
#' @param work_schedule Work schedule created with create_work_schedule
#' @param name Name of holiday
#' @param dates Dates of floating holiday. Make sure to list all dates relevant to your time frame
#'
#' @concept work_schedule
#'
#' @export

add_floating_holiday <- function(work_schedule, name, dates) {
	work_schedule$floating_holidays <- work_schedule$floating_holidays %>%
		bind_rows(tibble(name = name, date = dates))
	return(work_schedule)
}
#' Add holiday period to work schedule
#' @param work_schedule Work schedule created with create_work_schedule
#' @param from Start of holiday period (included)
#' @param to End of holiday period (included)
#'
#' @concept work_schedule
#'
#' @export

add_holiday_periods <- function(work_schedule, from, to) {
	work_schedule$holiday_periods <- work_schedule$holiday_periods %>%
		bind_rows(tibble(from = from, to = to))
	return(work_schedule)
}

get_holidays <- function(work_schedule, years) {

	month <- NULL
	day <- NULL
	from <- NULL
	to <- NULL

	work_schedule$fixed_holidays %>%
		full_join(tibble(year = years), by = character()) %>%
		mutate(date = make_date(year, month, day)) %>%
		pull(date) -> fixed

	work_schedule$floating_holidays %>%
		pull(date) -> floating

	work_schedule$holiday_periods %>%
		mutate(date = map2(from, to, seq.Date, by = 1)) %>%
		select(date) %>%
		unnest(date) %>%
		pull(date) -> periods


	return(c(fixed, floating, periods))
}
#' Print work schedule
#' @param x Work schedule to print
#' @param ... Additional arguments (ignored)
#'
#' @concept work_schedule
#'
#' @export

print.work_schedule <- function(x, ...) {
	cat("Week schedule\n")
	print(x$week_schedule)
	cat("\nFixed holidays\n")
	print(x$fixed_holidays)
	cat("\nFloating holidays\n")
	print(x$floating_holidays)
	cat("\nHoliday periods\n")
	print(x$holiday_periods)
}





