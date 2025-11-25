

plot_trace_coverage <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	relative <- NULL
	cum_sum <- NULL


	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes(relative)) +
			geom_histogram(bins = 20, boundary = 0, fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = "Relative trace coverage", y = "#traces") -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes(relative)) +
			geom_histogram(bins = 20, boundary = 0, fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = "Relative trace coverage", y = "#cases") -> p

	}
	else if(level == "trace") {
		x %>%
			mutate(id = row_number(cum_sum)/n()) %>%
			ggplot(aes(id, relative)) +
			geom_col(aes(fill = relative)) +
			geom_line(aes(id, cum_sum)) +
			theme_light() +
			labs(x = "Relative Number of traces", y = "(Cumulative) relative frequency") +
			scale_fill_continuous_bupaR(name = "Relative frequency", palette = "green") -> p
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
			scale_fill_continuous_bupaR(name = "Relative frequency", palette = "green") -> p
	}
	if(!is.null(mapping$groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
