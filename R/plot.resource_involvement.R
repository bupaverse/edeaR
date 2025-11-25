

plot_resource_involvement <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	absolute <- NULL
	relative <- NULL

	if(level == "case") {

		range <- max(x$absolute) - min(x$absolute)

		x %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			theme_light() +
			labs(x = "#resources",y = "#cases") -> p
	}
	else if(level == "resource-activity") {
		x %>%
			ggplot(aes_string(mapping$activity_id, mapping$resource_id)) +
			geom_tile(aes(fill = absolute)) +
			geom_text(aes(label = absolute), fontface = "bold", color = "white") +
			theme_light() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			scale_fill_continuous_bupaR(name = "#cases", palette = "green") +
			labs(x = "Activities",y = "Resources") -> p
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_bupaR(name = "#cases", palette = "green") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "#cases") -> p
	}

	if(!is.null(mapping$groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")
	}

	return(p)
}
