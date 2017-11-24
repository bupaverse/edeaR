plot_activity_presence <- function(x, ...) {

	mapping <- attr(x, "mapping")
	relative <- NULL

	x %>%
		ggplot(aes_string(glue::glue("reorder({mapping$activity_id}, relative)"), "relative")) +
		geom_col(aes(fill = relative)) +
		scale_fill_continuous_tableau(name = "Activity Presence", palette = "Blue")+
		theme_light() +
		coord_flip() +
		labs(x = "Activity", y = "Relative Activity Presence") -> p


	if(!is.null(attr(x, "groups"))) {
		p <-	p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
