

plot_trace_coverage <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	relative <- NULL
	cum_sum <- NULL


	if(level == "log") {
		stop("Plot only available at trace level")
	}
	else if(level == "case") {
		stop("Plot only available at trace level")

	}
	else if(level == "trace") {
		x %>%
			mutate(id = row_number(cum_sum)/n()) %>%
			ggplot(aes(id, relative)) +
			geom_col(aes(fill = relative)) +
			geom_line(aes(id, cum_sum)) +
			theme_light() +
			labs(x = "Relative Number of traces", y = "(Cumulative) relative frequency") +
			scale_fill_continuous_tableau(name = "Relative frequency", palette = "Blue") -> p
	}

	if(!is.null(attr(x, "groups"))) {
		x %>%
			group_by_(as.character(attr(x, "groups"))) %>%
			mutate(id = row_number(cum_sum)/n()) %>%
			ggplot(aes(id, relative)) +
			geom_col(aes(fill = relative)) +
			geom_line(aes(id, cum_sum)) +
			theme_light() +
			labs(x = "Relative Number of traces", y = "(Cumulative) relative frequency") +
			scale_fill_continuous_tableau(name = "Relative frequency", palette = "Blue") -> p
	}
	if(!is.null(attr(x, "groups"))) {
		p <- p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
