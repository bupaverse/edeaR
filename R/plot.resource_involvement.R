

plot_resource_involvement <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	absolute <- NULL
	relative <- NULL

if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, relative)"), "relative")) +
			geom_col(aes(fill = relative)) +
			scale_fill_continuous_tableau(name = "Relative number of resources involved", palette = "Blue") +
			coord_flip() +
			theme_light() +
			scale_x_discrete(breaks = NULL) +
			theme(axis.text.y = element_blank()) +
			labs(x = "Cases",y = "Relative number of resources") -> p
	}
	else if(level == "resource-activity") {
		x %>%
			ggplot(aes_string(mapping$activity_id, mapping$resource_id)) +
			geom_tile(aes(fill = absolute)) +
			geom_text(aes(label = absolute), fontface = "bold", color = "white") +
			theme_light() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			scale_fill_continuous_tableau("Number of cases resource-activity involved in", palette = "Blue") +
			labs(x = "Activities",y = "Resources") -> p
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Number of cases Resource involved in", palette = "Blue") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "Resource frequency") -> p
	}

	if(!is.null(attr(x, "groups"))) {
		p <- p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
