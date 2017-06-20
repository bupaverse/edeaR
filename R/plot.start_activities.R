#' @title Start Activities Plot
#'
#' @description  Visualize start activities data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot start_activities

#' @export

plot.start_activities <- function(x, ...) {
	data <- x

	mapping <- attr(data, "mapping")
	level <- attr(data, "level")


	if(level == "log") {
		stop("Plot not available for this level of analysis")
	}
	else if(level == "case") {
		stop("Plot not available for this level of analysis")
	}
	else if(level == "activity") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Start Activity Frequency", palette = "Blue")+
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = "Start Activity Frequency") -> p
	}
	else if(level == "resource") {
		data %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Start Activity Resource Frequency", palette = "Blue")+
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = "Start Activity Resource Frequency") -> p
	}
	else if(level == "resource-activity") {
		data %>%
			ggplot(aes_string(mapping$resource_id, mapping$activity_id)) +
			geom_tile(aes(fill = absolute)) +
			geom_text(aes(label = absolute), fontface = "bold", color = "white") +
			scale_fill_continuous_tableau(name = "Start Resource-Activity Frequency", palette = "Blue")+
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = "Activity") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
	}
	return(p)
}
