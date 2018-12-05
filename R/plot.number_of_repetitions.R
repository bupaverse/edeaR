

plot_number_of_repetitions <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")
	type <- attr(x,"type")
	absolute <- NULL



	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", absolute)) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = glue("Number of {type} repetitions (per case)")) -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(palette = "Blue", name = "Number of repetitions") +
			labs(x = "Cases", y = glue("Number of {type} repetitions")) +
			scale_y_continuous() +
			coord_flip() +
			theme_light() +
			theme(axis.text.x = element_blank()) +
			scale_x_discrete(breaks = NULL) -> p
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "Activity", y = glue("Number of {type} repetitions")) -> p
	}
	else if(level == "resource") {
		if(type %in% c("redo", "all")) {
			x %>%
				ggplot(aes_string(glue("reorder(first_resource, absolute)"), "absolute")) +
				geom_col(aes(fill = absolute)) +
				scale_y_continuous() +
				theme_light() +
				coord_flip() +
				labs(x = "First resource", y = glue("Number of {type} repetitions")) -> p
		}
		else if (type == "repeat") {
			x %>%
				ggplot(aes_string(glue("reorder({mapping$resource_id}, absolute)"), "absolute")) +
				geom_col(aes(fill = absolute)) +
				scale_y_continuous() +
				theme_light() +
				coord_flip() +
				labs(x = "Resource", y = glue("Number of {type} repetitions")) -> p
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
				scale_fill_continuous_tableau(glue("Number of {type} repetitions"), palette = "Blue") +
				labs(x = "Activity", y = "First resource") -> p
		}
		else if (type == "repeat") {
			x %>%
				ggplot(aes_string(mapping$activity_id, mapping$resource_id)) +
				geom_tile(aes(fill = absolute)) +
				geom_text(aes(label = absolute), fontface = "bold", color = "white") +
				theme_light() +
				theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
				scale_fill_continuous_tableau(glue("Number of {type} repetitions"), palette = "Blue") +
				labs(x = "Activity", y = "Resource") -> p
		}
	}

	if(!is.null(attr(x, "groups"))) {
		p <-	p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}


	return(p)
}
