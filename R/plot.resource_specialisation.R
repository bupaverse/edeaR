#' @title Resource Specialisation Plot
#'
#' @description  Visualize resource specialisation data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot resource_specialisation

#' @export

plot.resource_specialisation <- function(x, ...) {

	data <- x

	mapping <- attr(data, "mapping")
	level <- attr(data, "level")
	units <- attr(data, "units")

	if(level == "log") {
		attr(data, "raw") %>%
			ggplot(aes("", freq)) +
			geom_boxplot() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = "Number of activities performed per resource") -> p
	}
	else if(level == "case") {
		stop("No plot available at this level")
	}
	else if(level == "activity") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, relative)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			theme_light() +
			coord_flip() +
			scale_fill_continuous_tableau("Number of resources performing an activity", palette = "Blue") +
			labs(x = "Activities",y = "Number of resources") -> p
	}
	else if(level == "resource") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Number of activities executed per resource", palette = "Blue") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "Number of activities") -> p
	}
	return(p)
}
