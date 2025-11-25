

plot_resource_specialisation <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	absolute <- NULL
	freq <- NULL

	if(level == "log") {
		range <- max(attr(x, "raw")$freq) - min(attr(x, "raw")$freq)

		attr(x, "raw") %>%
			ggplot(aes(freq)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = glue("#activities"), y = "#resources") -> p
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, relative)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			theme_light() +
			coord_flip() +
			scale_fill_continuous_bupaR(name = "#resources", palette = "green") +
			labs(x = "Activities",y = "#resources") -> p
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_bupaR(name = "#activities", palette = "green") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "#activities") -> p
	}


	if(!is.null(mapping$groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")
	}


	return(p)
}
