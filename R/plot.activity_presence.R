plot_activity_presence <- function(x, ...) {

	mapping <- attr(x, "mapping")
	relative <- NULL

	x %>%
		ggplot(aes_string(glue::glue("reorder({mapping$activity_id}, relative)"), "relative")) +
		geom_col(aes(fill = relative)) +
		scale_fill_continuous_bupaR(name = "Activity Presence", palette = "green")+
		theme_light() +
		coord_flip() +
		labs(x = "Activity", y = "Relative Activity Presence") -> p


	if(!is.null(mapping$groups)) {
		p <-	p + facet_grid(as.formula(paste(c(paste(mapping$groups, collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
