#' @title Throughput time Plot
#'
#' @description  Visualize throughput time data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot throughput_time

#' @export

plot.throughput_time <- function(x, ...) {
	data <- x

	mapping <- attr(data, "mapping")
	level <- attr(data, "level")
	units <- attr(data, "units")


	if(level == "log") {
		attr(data, "raw") %>%
			ggplot(aes("", throughput_time)) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = glue("Throughput time (in {units})")) -> p
	}
	else if(level == "case") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, throughput_time)"), "throughput_time")) +
			geom_col(aes(fill = throughput_time)) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Throughput Time") +
			labs(x = "Cases", y = glue("Throughput time (in {units})")) +
			theme_light() +
			theme(axis.text.x = element_blank()) +
			scale_x_discrete(breaks = NULL) -> p
	}
	else if(level == "trace") {
		stop("Plot not available for this level of analysis")
	}
	return(p)
}
