

plot_number_of_selfloops <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")
	type <- attr(x,"type")
	absolute <- NULL


	if(level == "log") {
		range <- max(attr(x, "raw")$absolute) - min(attr(x, "raw")$absolute)

		attr(x, "raw") %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = glue("#selsfloops (per case)"), y = "#cases") -> p

	}
	else if(level == "case") {
		range <- max(x$absolute) - min(x$absolute)

		x %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 20, 20, range+1), fill = col_vector()[1], color = "white") +
			scale_x_continuous() +
			theme_light() +
			labs(x = glue("#selfloops (per case)"), y = "#cases") -> p
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_y_continuous() +
			scale_fill_continuous_bupaR(palette = "green", name = "#selfloops") +
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = glue("#selfloops")) -> p
	}
	else if(level == "resource") {
		if(type %in% c("redo", "all")) {
			x %>%
				ggplot(aes_string(glue("reorder(first_resource, absolute)"), "absolute")) +
				geom_col(aes(fill = absolute)) +
				scale_y_continuous() +
				scale_fill_continuous_bupaR(palette = "green", name = "#selfloops") +
				theme_light() +
				coord_flip() +
				labs(x = "First resource", y = glue("#selfloops")) -> p
		}
		else if (type == "repeat") {
			x %>%
				ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
				geom_col(aes(fill = absolute)) +
				scale_y_continuous() +
				scale_fill_continuous_bupaR(palette = "green", name = "#selfloops") +
				theme_light() +
				coord_flip() +
				labs(x = "Resource", y = glue("#selfloops")) -> p
		}
	}
	else if(level == "resource-activity") {
		if(type %in% c("redo", "all")) {
			x %>%
				ggplot(aes_string(mapping$activity_id, "first_resource")) +
				geom_tile(aes(fill = absolute)) +
				geom_text(aes(label = absolute), fontface = "bold", color = "white") +
				theme_light() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
				scale_fill_continuous_bupaR(palette = "green", name = "#selfloops") +
				labs(x = "Activity", y = "First resource") -> p
		}
		else if (type %in% c("repeat")) {
			x %>%
				ggplot(aes_string(mapping$activity_id, mapping$resource_id)) +
				geom_tile(aes(fill = absolute)) +
				geom_text(aes(label = absolute), fontface = "bold", color = "white") +
				theme_light() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
				scale_fill_continuous_bupaR(palette = "green", name = "#selfloops") +
				labs(x = "Activity", y = "Resource") -> p
		}
	}

	if(!is.null(mapping$groups)) {
		p <-	p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y", space = "free")
	}


	return(p)
}
