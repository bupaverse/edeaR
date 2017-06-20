#' @title Referral Matrix Plot
#'
#' @description  Visualize referral matrix
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot referral_matrix

#' @export

plot.referral_matrix <- function(x, ...) {

	data <- x

	mapping <- attr(data, "mapping")
	type <- attr(data, "type")


	data %>%
		ggplot(aes(first_resource, last_resource)) +
		geom_tile(aes(fill = absolute)) +
		geom_text(aes(label = absolute), fontface = "bold", color = "white") +
		labs(x = glue("First resource of {type}"), y = glue("Last resource of {type})")) +
		scale_fill_continuous_tableau(name = "Frequency", palette = "Blue") %>% return
}
