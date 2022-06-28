activity_frequency_case <- function(log) {
	absolute <- NULL
	relative <- NULL
	n_labels <- NULL
	n_instances <- NULL

	log %>%
		group_by(.data[[case_id(.)]], .data[[activity_id(.)]], .data[[activity_instance_id(.)]]) %>%
		#summarize() %>%
		summarize("n" = n()) %>%
		group_by(.data[[case_id(log)]]) %>%
		summarize("n_instances" = sum(n),
				  "n_labels" = n()) %>%
		transmute(.data[[case_id(log)]],
				  "absolute" = .data[["n_labels"]],
				  "relative" = .data[["n_labels"]] / .data[["n_instances"]])
}
