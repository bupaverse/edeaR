
plot_activity_frequency <- function(x, ...) {

	mapping <- attr(x, "mapping")
	level <- attr(x, "level")

	absolute <- NULL


	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", absolute)) +
			geom_boxplot() +
			coord_flip() +
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
			coord_flip() +
			theme(axis.text.x = element_blank()) -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, absolute)"), "absolute")) +
			geom_col(aes(fill = absolute)) +
			scale_fill_continuous_tableau(name = "Activity Frequency", palette = "Blue")+
					theme_light() +
			scale_x_discrete(breaks = NULL) +
			coord_flip() +
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
			labs(x = "Activities", y = "Absolute Activity Frequency") -> p
	}

	if(!is.null(attr(x, "groups"))) {
		p <-	p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
