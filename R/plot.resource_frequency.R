#' @title Resource frequency Plot
#'
#' @description  Visualize resource frequency data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot resource_frequency

#' @export

plot.resource_frequency <- function(x, ...) {

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
			labs(x = "", y = "Resource frequency") -> p
	}
	else if(level == "case") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, nr_of_resources)"), "nr_of_resources")) +
			geom_col(aes(fill = nr_of_resources)) +
			scale_fill_continuous_tableau(name = "Resource frequency", palette = "Blue") +
			coord_flip() +
			theme_light() +
			scale_x_discrete(breaks = NULL) +
			theme(axis.text.y = element_blank()) +
			labs(x = "Cases",y = "Resource frequency") -> p
	}
	else if(level == "activity") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, nr_of_resources)"), "nr_of_resources")) +
			geom_col(aes(fill = nr_of_resources)) +
			scale_fill_continuous_tableau(name = "Resource frequency", palette = "Blue") +
			coord_flip() +
			theme_light() +
			labs(x = "Activities",y = "Resource frequency") -> p
	}
	else if(level == "resource-activity") {
		data %>%
			ggplot(aes_string(mapping$activity_id, mapping$resource_id)) +
			geom_tile(aes(fill = absolute)) +
			geom_text(aes(label = absolute), fontface = "bold", color = "white") +
			theme_light() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			scale_fill_continuous_tableau("Resource-activity Frequency", palette = "Blue") +
			labs(x = "Activities",y = "Resources") -> p
	}
	else if(level == "resource") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Resource frequency", palette = "Blue") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "Resource frequency") -> p
	}
	return(p)
}
