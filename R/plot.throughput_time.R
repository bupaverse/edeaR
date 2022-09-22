

plot_throughput_time <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	y_lab <- glue("Throughput time (in {units})")

	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", throughput_time)) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			scale_y_continuous() +
			labs(x = "", y = y_lab) -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, throughput_time)"), "throughput_time")) +
			geom_col(aes(fill = as.numeric(throughput_time))) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Throughput Time") +
			scale_y_continuous() +
			labs(x = "Cases", y = y_lab) +
			theme_light() +
			coord_flip() +
			theme(axis.text.y = element_blank()) +
			scale_x_discrete(breaks = NULL) -> p
	}
	else if(level == "trace") {
		stop("Plot not available for this level of analysis")
	}



	if(!is.null(mapping$groups)) {
		p <-	p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")))
	}

	return(p)
}
