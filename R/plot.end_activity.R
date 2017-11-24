
plot_end_activities <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	absolute <- NULL


	if(level == "log") {
		stop("Plot not available for this level of analysis")
	}
	else if(level == "case") {
		stop("Plot not available for this level of analysis")
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "End Activity Frequency", palette = "Blue")+
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = "End Activity Frequency") -> p
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "End Activity Resource Frequency", palette = "Blue")+
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = "End Activity Resource Frequency") -> p
	}
	else if(level == "resource-activity") {
		x %>%
			ggplot(aes_string(mapping$resource_id, mapping$activity_id)) +
			geom_tile(aes(fill = absolute)) +
			geom_text(aes(label = absolute), fontface = "bold", color = "white") +
			scale_fill_continuous_tableau(name = "End Resource-Activity Frequency", palette = "Blue")+
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = "Activity") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
	}

	if(!is.null(attr(x, "groups"))) {
		p <- p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}


	return(p)
}
