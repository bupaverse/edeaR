#' @title Trace Length Plot
#'
#' @description  Visualize trace length data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot trace_length

#' @export

plot.trace_length <- function(x, ...) {
	data <- x

	mapping <- attr(data, "mapping")
	level <- attr(data, "level")

	if(level == "log") {
		attr(data, "raw") %>%
			ggplot(aes("", trace_length)) +
			geom_boxplot()+
			labs(x = "", y = "Trace length") +
			theme_light() -> p
	}
	else if(level == "case") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, trace_length)"), "trace_length")) +
			geom_col(aes(fill = trace_length)) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Trace length per case") +
			theme_light() +
			theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
			labs(x = "Cases", y = "Trace length") -> p

	}
	else if(level == "trace") {
		data %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = 20, color = "black", fill = "white") +
			labs(x = "Trace length", y = "Number of traces") +
			theme_light() -> p

	}

	return(p)
}
