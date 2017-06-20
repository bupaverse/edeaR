#' @title PRocessing time Plot
#'
#' @description  Visualize process time data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot processing_time

#' @export

plot.processing_time <- function(x, ...) {

	data <- x

	mapping <- attr(data, "mapping")
	level <- attr(data, "level")
	units <- attr(data, "units")


	if(level == "log") {
		attr(data, "raw") %>%
			ggplot(aes("", processing_time)) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = glue("Processing time (in {units})")) -> p
	}
	else if(level == "case") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, processing_time)"), "processing_time")) +
			geom_col(aes(fill = processing_time)) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Processing Time") +
			labs(x = "Cases", y = glue("Processing time (in {units})")) +
			theme_light() +
			theme(axis.text.x = element_blank()) +
			scale_x_discrete(breaks = NULL) -> p
	}
	else if(level == "trace") {
		stop("Plot not available for this level of analysis")
	}
	else if(level == "activity") {
		attr(data, "raw") %>%
			ggplot(aes_string(mapping$activity_id, "processing_time")) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = glue("Processing time (in {units})")) -> p
	}
	else if(level == "resource") {
		attr(data, "raw") %>%
			ggplot(aes_string(mapping$resource_id, "processing_time")) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = glue("Processing time (in {units})")) -> p
	}
	else if(level == "resource-activity") {
		attr(data, "raw") %>%
			ggplot(aes_string(mapping$activity_id, "processing_time")) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			facet_wrap(mapping$resource_id) +
			labs(x = "Activity", y = glue("Processing time (in {units})")) -> p
	}
	return(p)
}
