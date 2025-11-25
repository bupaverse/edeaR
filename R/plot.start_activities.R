

plot_start_activities <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	absolute <- NULL


	if(level == "log") {
		stop("Plot not available for this level of analysis")
	}
	else if(level == "case") {
		if(!is.null(mapping$groups)) {
			x %>%
				count(across(c(mapping$groups, mapping$activity_id))) %>%
				ggplot(aes_string(glue("fct_infreq({mapping$activity_id}, n)"), "n", fill = "n")) +
				geom_col() +
				scale_fill_continuous_bupaR(name = "#cases", palette = "green")+
				theme_light() +
				coord_flip() +
				labs(x = "Activity", y = "#cases") -> p
		} else {
			x %>%
				count(across(c(mapping$activity_id))) %>%
				ggplot(aes_string(glue("fct_infreq({mapping$activity_id}, n)"), "n", fill = "n")) +
				geom_col() +
				scale_fill_continuous_bupaR(name = "#cases", palette = "green")+
				theme_light() +
				coord_flip() +
				labs(x = "Activity", y = "#cases") -> p
		}
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_bupaR(name = "#cases", palette = "green")+
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = "#cases") -> p
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_bupaR(name = "#cases", palette = "green")+
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = "#cases") -> p
	}
	else if(level == "resource-activity") {
		x %>%
			ggplot(aes_string(mapping$resource_id, mapping$activity_id)) +
			geom_tile(aes(fill = absolute)) +
			geom_text(aes(label = absolute), fontface = "bold", color = "white") +
			scale_fill_continuous_bupaR(name = "#cases", palette = "green")+
			theme_light() +
			coord_flip() +
			labs(x = "Resource", y = "Activity") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1)) -> p
	}

	if(!is.null(mapping$groups)) {
		p <- p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}
	return(p)
}
