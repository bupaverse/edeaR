#' @title Idle Time Plot
#'
#' @description  Visualize idle time data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot idle_time

#' @export

plot.idle_time <- function(x, ...) {

	data <- x

	mapping <- attr(data, "mapping")
	level <- attr(data, "level")
	units <- attr(data, "units")

	if(level == "log") {
		attr(data, "raw") %>%
			ggplot(aes("", idle_time)) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = "Idle time per case") -> p
	}
	else if(level == "case") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, idle_time)"), "idle_time")) +
			geom_col(aes(fill = idle_time)) +
			scale_fill_continuous_tableau(name = glue("Idle time per case (in {units})"), palette = "Blue") +
			theme_light() +
			theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
			labs(x = "Cases", y = "Idle time") -> p
	}
	else if(level == "trace") {
		stop("No plot availabe at this level")
	}
	else if(level == "resource") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, idle_time)"), "idle_time")) +
			geom_col(aes(fill = idle_time)) +
			scale_fill_continuous_tableau(name = glue("Idle time (in {units})"), palette = "Blue") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "Idle time") -> p
	}
	return(p)
}
