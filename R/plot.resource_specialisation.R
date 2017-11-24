

plot_resource_specialisation <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	absolute <- NULL
	freq <- NULL

	if(level == "log") {
		attr(x, "raw") %>%
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
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, relative)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			theme_light() +
			coord_flip() +
			scale_fill_continuous_tableau("Number of resources performing an activity", palette = "Blue") +
			labs(x = "Activities",y = "Number of resources") -> p
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Number of activities executed per resource", palette = "Blue") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "Number of activities") -> p
	}


	if(!is.null(attr(x, "groups"))) {
		p <- p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}


	return(p)
}
