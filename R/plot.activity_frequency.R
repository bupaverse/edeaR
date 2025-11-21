
plot_activity_frequency <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")

	absolute <- NULL


	if(level == "log") {

		range <- x$max - x$min

		attr(x, "raw") %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 30, 30, range+1), fill = bupaR:::col_vector()[1], color = "white") +
			theme_light() +
			labs(x = "Absolute Activity Frequency", y = "#cases") -> p
	}
	else if(level == "trace") {
		range <- max(x$absolute) - min(x$absolute)
		x %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 30, 30, range+1), fill = bupaR:::col_vector()[1], color = "white") +
			theme_light() +
			labs(x = "Absolute Activity Frequency", y = "#traces") -> p
	}
	else if(level == "case") {
		range <- max(x$absolute) - min(x$absolute)
		x %>%
			ggplot(aes(absolute)) +
			geom_histogram(bins = ifelse(range > 30, 30, range+1), fill = bupaR:::col_vector()[1], color = "white") +
			theme_light() +
			labs(x = "Absolute Activity Frequency", y = "#cases") -> p
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_bupaR(name = "Activity Frequency", palette = "green")+
			theme_light() +
			coord_flip() +
			labs(x = "Activities", y = "Absolute Activity Frequency") -> p
	}

	if(!is.null(mapping$groups)) {
		p <-	p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
