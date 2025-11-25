

plot_idle_time <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	y_lab <- glue("Idle time (in {units})")

	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes(idle_time)) +
			geom_histogram(bins = 20, fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = y_lab, y = "#cases") -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes(idle_time)) +
			geom_histogram(bins = 20, fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = y_lab, y = "#cases") -> p
	}
	else if(level == "trace") {
		stop("No plot availabe at this level")
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, idle_time)"), "idle_time")) +
			geom_col(aes(fill = as.numeric(idle_time))) +
			scale_fill_continuous_bupaR(name = y_lab, palette = "green") +
			scale_y_continuous() +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = y_lab) -> p
	}
	if(!is.null(mapping$groups)) {
		p <-	p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")
	}

	return(p)
}
