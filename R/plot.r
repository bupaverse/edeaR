#' @title Plot Methods
#'
#' @description  Visualize metric
#' @param x Data to plot
#' @param ... Additional variables
#' @return A ggplot object, which can be customized further, if deemed necessary.
#' @name plot
#' @method plot activity_frequency
#' @export

plot.activity_frequency <- function(x, ...) {

	return(plot_activity_frequency(x, ...))
}

#' @rdname plot
#' @method plot activity_presence
#' @export
#'
plot.activity_presence <- function(x, ...) {

	return(plot_activity_presence(x, ...))
}

#' @rdname plot
#' @method plot end_activities
#' @export

plot.end_activities <- function(x, ...) {
	return(plot_end_activities(x, ...))
}

#' @rdname plot
#' @method plot idle_time
#' @export

plot.idle_time <- function(x, ...) {
	return(plot_idle_time(x, ...))
}


#' @rdname plot
#' @method plot processing_time
#' @export

plot.processing_time <- function(x, ...) {

	return(plot_processing_time(x, ...))
}


#' @rdname plot
#' @method plot referral_matrix
#' @export

plot.referral_matrix <- function(x, ...) {
	return(plot_referral_matrix(x, ...))
}

#' @rdname plot
#' @method plot resource_frequency
#' @export

plot.resource_frequency <- function(x, ...) {
	return(plot_resource_frequency(x, ...))
}
#' @rdname plot
#' @method plot resource_involvement
#' @export

plot.resource_involvement <- function(x, ...) {
	return(plot_resource_involvement(x,...))
}


#' @rdname plot
#' @method plot resource_specialisation
#' @export

plot.resource_specialisation <- function(x, ...) {
	return(plot_resource_specialisation(x,...))
}
#' @rdname plot
#' @method plot start_activities
#' @export

plot.start_activities <- function(x, ...) {
	return(plot_start_activities(x, ...))
}

#' @rdname plot
#' @method plot throughput_time
#' @export

plot.throughput_time <- function(x, ...) {
	return(plot_throughput_time(x, ...))
}

#' @rdname plot
#' @method plot trace_coverage
#' @export

plot.trace_coverage <- function(x, ...) {
	return(plot_trace_coverage(x, ...))
}

#' @rdname plot
#' @method plot trace_length
#' @export

plot.trace_length <- function(x, ...) {
	return(plot_trace_length(x, ...))
}

#' @rdname plot
#' @method plot number_of_selfloops
#' @export
#'
plot.number_of_selfloops <- function(x, ...) {
	return(plot_number_of_selfloops(x, ...))
}

#' @rdname plot
#' @method plot number_of_repetitions
#' @export
plot.number_of_repetitions <- function(x, ...) {
	return(plot_number_of_repetitions(x, ...))
}
