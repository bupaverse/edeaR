#' @title Activity Presence Plot
#'
#' @description  Visualize activity presence data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot activity_presence

#' @export

plot.activity_presence <- function(x, ...) {

	mapping <- attr(x, "mapping")

	x %>%
		ggplot(aes_string(glue::glue("reorder({mapping$activity_id}, relative)"), "relative")) +
		geom_col(aes(fill = relative)) +
		scale_fill_continuous_tableau(name = "Activity Presence", palette = "Blue")+
		theme_light() +
		coord_flip() +
		labs(x = "Activity", y = "Relative Activity Presence") -> p
	return(p)
}
