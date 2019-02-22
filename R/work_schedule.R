

#' Create work schedule
#'
#' @param start_time Character indicating the usual start time for workdays (hh:mm:ss)
#' @param end_time Character indicating the usual end time for workdays (hh:mm:ss)
#'
#' @export
#'
create_work_schedule <- function(start_time = "9:00:00", end_time = "17:00:00") {
	tibble(day = 1:7,
		   start_time = parse_hms(c(rep(start_time, 5), NA, NA)),
		   end_time = parse_hms(c(rep(end_time, 5), NA, NA))
		   ) -> work_schedule
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
#' @export
#'
change_day <- function(work_schedule, day, start_time, end_time) {
	if(!("work_schedule") %in% class(work_schedule)) {
		stop("work_schedule should be created with function create_work_schedule")
	}
	work_schedule$start_time[work_schedule$day %in% day] <- parse_hms(start_time)
	work_schedule$end_time[work_schedule$day %in% day] <- parse_hms(end_time)
	return(work_schedule)
}


