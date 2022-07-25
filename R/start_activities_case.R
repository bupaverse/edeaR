start_activities_case <- function(log) {

	log %>%
		as.data.frame() %>%
		group_by(!!case_id_(log)) %>%
		arrange(!!as.symbol(timestamp(log)), .order) %>%
		summarize(!!as.symbol(activity_id(log)) := first(!!as.symbol(activity_id(log)))) %>%
		select(!!as.symbol(case_id(log)), !!as.symbol(activity_id(log)))
}
