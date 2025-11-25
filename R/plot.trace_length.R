

plot_trace_length <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")

	absolute <- NULL


	if(level == "log") {
		range <- max(attr(x, "raw")$absolute) - min(attr(x, "raw")$absolute)

		attr(x, "raw") %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			theme_light() +
			labs(x = "Trace length", y = "#cases") -> p
	}
	else if(level == "case") {
		range <- max(x$absolute) - min(x$absolute)

		x %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			theme_light() +
			labs(x = "Trace length", y = "#cases") -> p

	}
	else if(level == "trace") {
		range <- max(x$absolute) - min(x$absolute)

		x %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			labs(x = "Trace length", y = "#traces") +
			theme_light() -> p

	}

	if(!is.null(mapping$groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")
	}


	return(p)
}
