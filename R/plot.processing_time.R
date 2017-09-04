

plot_processing_time <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")


	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", processing_time)) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = glue("Processing time (in {units})")) -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, processing_time)"), "processing_time")) +
			geom_col(aes(fill = processing_time)) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Processing Time") +
			labs(x = "Cases", y = glue("Processing time (in {units})")) +
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
			labs(x = "Activity", y = glue("Processing time (in {units})")) -> p
	}
	else if(level == "resource") {
		attr(x, "raw") %>%
			ggplot(aes_string(mapping$resource_id, "processing_time")) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = glue("Processing time (in {units})")) -> p
	}
	else if(level == "resource-activity") {
		attr(x, "raw") %>%
			ggplot(aes_string(mapping$activity_id, "processing_time", color = mapping$resource_id)) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = glue("Processing time (in {units})")) -> p
	}

	if(!is.null(attr(x, "groups"))) {
		p <-	p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}


	return(p)
}
