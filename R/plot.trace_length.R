

plot_trace_length <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")

	absolute <- NULL


	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", absolute)) +
			geom_boxplot()+
			labs(x = "", y = "Trace length") +
			theme_light() +
			coord_flip() -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Trace length per case") +
			theme_light() +
			coord_flip() +
			theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
			labs(x = "Cases", y = "Trace length") -> p

	}
	else if(level == "trace") {
		x %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = 20, color = "black", fill = "white") +
			labs(x = "Trace length", y = "Number of traces") +
			theme_light() -> p

	}

	if(!is.null(attr(x, "groups"))) {
		p <- p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}


	return(p)
}
