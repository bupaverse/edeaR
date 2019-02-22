

plot_idle_time <- function(x, ...) {


	mapping <- attr(x, "mapping")
	level <- attr(x, "level")
	units <- attr(x, "units")

	if(level == "log") {
		attr(x, "raw") %>%
			ggplot(aes("", idle_time)) +
			geom_boxplot() +
			scale_y_continuous() +
			theme_light() +
			coord_flip() +
			labs(x = "", y = "Idle time per case (in {units})") -> p
	}
	else if(level == "case") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$case_id}, idle_time)"), "idle_time")) +
			geom_col(aes(fill = idle_time)) +
			scale_fill_continuous_tableau(name = glue("Idle time per case (in {units})"), palette = "Blue") +
			theme_light() +
			scale_y_continuous() +
			theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
			labs(x = "Cases", y = "Idle time (in {units})") -> p
	}
	else if(level == "trace") {
		stop("No plot availabe at this level")
	}
	else if(level == "resource") {
		x %>%
			ggplot(aes_string(glue("reorder({mapping$resource_id}, idle_time)"), "idle_time")) +
			geom_col(aes(fill = idle_time)) +
			scale_fill_continuous_tableau(name = glue("Idle time (in {units})"), palette = "Blue") +
			scale_y_continuous() +
			coord_flip() +
			theme_light() +
			labs(x = "Resources",y = "Idle time (in {units})") -> p
	}


	if(!is.null(attr(x, "groups"))) {
		p <-	p + facet_grid(as.formula(paste(c(paste(attr(x, "groups"), collapse = "+"), "~." ), collapse = "")), scales = "free_y")
	}

	return(p)
}
