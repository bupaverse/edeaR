#' @title Activity Frequency Plot
#'
#' @description  Visualize activity freqeuncy data.
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @method plot activity_frequency

#' @export

plot.activity_frequency <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")


	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", absolute)) +
			geom_boxplot() +
			theme_light() +
			labs(x = "", y = "Absolute Activity Frequency") -> p
	}
	else if(level == "trace") {
		x %>%
			ggplot(aes_string(glue("reorder(trace, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Activity Frequency", palette = "Blue")+
			theme_light() +
			scale_x_discrete(breaks = NULL) +
			labs(x = "Traces", y = "Absolute Activity Frequency") +
			theme(axis.text.x = element_blank()) -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Activity Frequency", palette = "Blue")+
					theme_light() +
			scale_x_discrete(breaks = NULL) +
			labs(x = "Cases", y = "Absolute Activity Frequency") +
			theme(axis.text.x = element_blank()) -> p
	}
	else if(level == "activity") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$activity_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Activity Frequency", palette = "Blue")+
			theme_light() +
			coord_flip() +
			labs(x = "Cases", y = "Absolute Activity Frequency") -> p
	}
	return(p)
}
