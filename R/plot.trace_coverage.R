#' @title Trace Coverage Plot
#'
#' @description  Visualize trace coverage data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot trace_coverage

#' @export

plot.trace_coverage <- function(x, ...) {
	data <- x

	mapping <- attr(data, "mapping")
	level <- attr(data, "level")
	units <- attr(data, "units")

	if(level == "log") {
		stop("Plot only available at case level")
	}
	else if(level == "case") {
		stop("Plot only available at case level")

	}
	else if(level == "trace") {
		data %>%
			mutate(id = row_number(cum_sum)/n()) %>%
			ggplot(aes(id, relative)) +
			geom_col(aes(fill = relative)) +
			geom_line(aes(id, cum_sum)) +
			theme_light() +
			labs(x = "Relative Number of traces", y = "(Cumulative) relative frequency") +
			scale_fill_continuous_tableau(name = "Relative frequency", palette = "Blue") -> p
	}

	return(p)
}
