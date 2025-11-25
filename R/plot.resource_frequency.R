

plot_resource_frequency <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")
	freq <- NULL
	nr_of_resources <- NULL
	absolute <- NULL

	if(level == "log") {
		range <- max(attr(x, "raw")$freq) - min(attr(x, "raw")$freq)

		attr(x, "raw") %>%
			ggplot(aes(freq)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = glue("#instances"), y = "#resources") -> p
	}
	else if(level == "case") {
		range <- max(x$nr_of_resources) - min(x$nr_of_resources)

		x %>%
			ggplot(aes(nr_of_resources)) +
			geom_histogram(fill = col_vector()[1], bins = ifelse(range > 20, 20, range+1), color = "white") +
			theme_light() +
			labs(x = "#resources",y = "#cases") -> p
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, nr_of_resources)"), "nr_of_resources")) +
			geom_col(aes(fill = nr_of_resources)) +
			scale_fill_continuous_bupaR(name = "#resources", palette = "green") +
			coord_flip() +
			theme_light() +
			labs(x = "Activities",y = "#resources") -> p
	}
	else if(level == "resource-activity") {
		x %>%
			ggplot(aes_string(mapping$activity_id, mapping$resource_id)) +
			geom_tile(aes(fill = absolute)) +
			geom_text(aes(label = absolute), fontface = "bold", color = "white") +
			theme_light() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
			scale_fill_continuous_bupaR(name = "#activity instances", palette = "green") +
			labs(x = "Activities",y = "Resources") -> p
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_bupaR(name = "#activity instances", palette = "green") +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "#activity instances") -> p
	}

	if(!is.null(mapping$groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
