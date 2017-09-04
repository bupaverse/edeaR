

plot_throughput_time <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")


	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", throughput_time)) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			scale_y_continuous() +
			labs(x = "", y = glue("Throughput time (in {units})")) -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, throughput_time)"), "throughput_time")) +
			geom_col(aes(fill = throughput_time)) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Throughput Time") +
			scale_y_continuous() +
			labs(x = "Cases", y = glue("Throughput time (in {units})")) +
			theme_light() +
			coord_flip() +
			theme(axis.text.x = element_blank()) +
			scale_x_discrete(breaks = NULL) -> p
	}
	else if(level == "trace") {
		stop("Plot not available for this level of analysis")
	}



	if(!is.null(attr(x, "groups"))) {
		p <-	p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")))
	}

	return(p)
}
