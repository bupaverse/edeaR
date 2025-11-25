

plot_throughput_time <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	y_lab <- glue("Throughput time (in {units})")

	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes(throughput_time)) +
			geom_histogram(bins = 20, fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = y_lab, y = "#cases") -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes(throughput_time)) +
			geom_histogram(bins = 20, fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = y_lab, y = "#cases") -> p
	}
	else if(level == "trace") {
		stop("Plot not available for this level of analysis")
	} else if(level == "activity") {
		attr(x, "raw") %>%
			ggplot(aes_string(mapping$activity_id, "throughput_time")) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = y_lab) -> p
	} else if(level == "activity-instance") {
		x %>%
			ggplot(aes(throughput_time)) +
			geom_histogram(bins = 20, fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = y_lab, y = "#activity instances") -> p
	}



	if(!is.null(mapping$groups)) {
		p <-	p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), space = "free")
	}

	return(p)
}
