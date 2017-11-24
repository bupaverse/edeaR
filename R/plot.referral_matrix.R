

plot_referral_matrix <- function(x, ...) {

	data <- x
	first_resource <- NULL
	last_resource <- NULL
	absolute <- NULL
	mapping <- attr(data, "mapping")
	type <- attr(data, "type")


	data %>%
		ggplot(aes(first_resource, last_resource)) +
		geom_tile(aes(fill = absolute)) +
		geom_text(aes(label = absolute), fontface = "bold", color = "white") +
		labs(x = glue("First resource of {type}"), y = glue("Last resource of {type})")) +
		scale_fill_continuous_tableau(name = "Frequency", palette = "Blue") %>% return
}
