

plot_processing_time <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	y_lab <- glue("Processing time (in {units})")

	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", processing_time)) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = y_lab) -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, processing_time)"), "processing_time")) +
			geom_col(aes(fill = as.numeric(processing_time))) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Processing Time") +
			labs(x = "Cases", y = y_lab) +
			scale_y_continuous() +
			theme_light() +
			theme(axis.text.x = element_blank()) +
			scale_x_discrete(breaks = NULL) -> p
	}
	else if(level == "trace") {
		stop("Plot not available for this level of analysis")
	}
	else if(level == "activity") {
		attr(x, "raw") %>%
			ggplot(aes_string(mapping$activity_id, "processing_time")) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = y_lab) -> p
	}
	else if(level == "resource") {
		attr(x, "raw") %>%
			ggplot(aes_string(mapping$resource_id, "processing_time")) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = y_lab) -> p
	}
	else if(level == "resource-activity") {
		attr(x, "raw") %>%
			ggplot(aes_string(mapping$activity_identifier, "processing_time", color = mapping$resource_identifier)) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = y_lab) -> p
	}

	if(!is.null(mapping$groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
