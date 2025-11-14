#' @title Filter Precedence Relations with Identical Resources
#'
#' @description Filters cases based on the precedence relations between two sets of activities, where both antecendent
#' and consequent have to be executed by the same resource.
#'
#' @inherit filter_precedence params references seealso return
#'
#' @family filters
#'
#' @seealso [`filter_precedence()`]
#'
#' @concept filters_case
#'
#' @export filter_precedence_resource
filter_precedence_resource <- function(log,
                                       antecedents,
                                       consequents,
                                       precedence_type = c("directly_follows", "eventually_follows"),
                                       filter_method = c("all", "one_of", "none"),
                                       reverse = FALSE) {
  UseMethod("filter_precedence_resource")
}

#' @describeIn filter_precedence_resource Filters cases for an [`eventlog`][`bupaR::eventlog`].
#' @export
filter_precedence_resource.eventlog <- function(log,
                                           antecedents,
                                           consequents,
                                           precedence_type = c("directly_follows", "eventually_follows"),
                                           filter_method = c("all", "one_of", "none"),
                                           reverse = FALSE) {

  precedence_type <- arg_match(precedence_type)
  filter_method <- arg_match(filter_method)

  n_fitting <- NULL

  # TEST VALIDITY ACTIVITY LABELS
  acts <- activity_labels(log)
  wrong <- antecedents[!(antecedents %in% acts)]

  if(length(wrong) > 0) {
    cli_warn(c("{.val {length(wrong)}} specified {.arg antecedents} not found in {.arg log}.",
               "!" = "Removed: {.val {wrong}}."))
    antecedents <- antecedents[(antecedents %in% acts)]
  }

  wrong <- consequents[!(consequents %in% acts)]

  if(length(wrong) > 0) {
    cli_warn(c("{.val {length(wrong)}} specified {.arg consequents} not found in {.arg log}.",
               "!" = "Removed: {.val {wrong}}."))
    consequents <- consequents[(consequents %in% acts)]
  }

  if(length(antecedents) < 1 || length(consequents) < 1) {
    cli_abort("No valid antecendent-consequent pairs.")
  }

  # CREATE FLOWS TO BE CHECKED
  sequence_pairs <- crossing(antecedent = antecedents, consequent = consequents)
  number_of_conditions <- nrow(sequence_pairs)

  output <- list_along(1:number_of_conditions)

  ## CHECK EACH FLOW
  for(i in 1:number_of_conditions) {
    output[[i]] <- filter_precedence_resource_single(log,
                                                     sequence_pairs$antecedent[i],
                                                     sequence_pairs$consequent[i],
                                                     precedence_type = precedence_type)
  }

  # COMPUTE NUMBER OF FLOWS FOUND
  output %>%
    bind_rows() %>%
    count(!!case_id_(log), name = "n_fitting") -> cases_results

  # CREATE CASE SELECTION
  if(filter_method == "one_of")
    case_selection <- filter(cases_results, n_fitting > 0) %>% pull(!!as.symbol(case_id(log)))
  else if(filter_method == "all")
    case_selection <- filter(cases_results, n_fitting == number_of_conditions) %>% pull(!!as.symbol(case_id(log)))
  else if(filter_method == "none")
    case_selection <- filter(cases_results, n_fitting == 0) %>% pull(!!as.symbol(case_id(log)))

  #FILTER CASES
  filter_case.log(log, cases = case_selection, reverse = reverse)
}

#' @describeIn filter_precedence_resource Filters cases for an [`activitylog`][`bupaR::activitylog`].
#' @export
filter_precedence_resource.activitylog <- function(log,
                                                   antecedents,
                                                   consequents,
                                                   precedence_type = c("directly_follows", "eventually_follows"),
                                                   filter_method = c("all", "one_of", "none"),
                                                   reverse = FALSE) {


  log %>%
    to_eventlog() %>%
    mutate(!!activity_instance_id(.) := as.character(.data[[activity_instance_id(.)]])) %>%
    filter_precedence_resource(antecedents = antecedents,
                                        consequents = consequents,
                                        precedence_type = precedence_type,
                                        filter_method = filter_method,
                                        reverse = reverse) %>%
    to_activitylog()
}

#' @describeIn filter_precedence_resource Filters cases for a [`grouped_log`][`bupaR::grouped_log`].
#' @export
filter_precedence_resource.grouped_log <- function(log,
                                                   antecedents,
                                                   consequents,
                                                   precedence_type = c("directly_follows", "eventually_follows"),
                                                   filter_method = c("all", "one_of", "none"),
                                                   reverse = FALSE) {

  bupaR:::apply_grouped_fun(log, fun = filter_precedence_resource, antecedents, consequents, precedence_type, filter_method, reverse, .ignore_groups = FALSE, .keep_groups = TRUE, .returns_log = TRUE)
  #grouped_filter(eventlog, filter_precedence_resource, antecedents, consequents, precedence_type, filter_method, reverse)
}


filter_precedence_resource_single <- function(eventlog, antecedent, consequent, precedence_type) {
  x <- NULL
  y <- NULL
  cons <- NULL
  ante <- NULL

  eventlog %>%
    create_minimal_activity_log() %>%
    group_by(!!case_id_(eventlog)) %>%
    arrange(!!timestamp_(eventlog), .order) %>%
    mutate(rank = 1:n()) %>%
    ungroup() %>%
    mutate(antecedent = (!!activity_id_(eventlog) == antecedent)*rank,
           consequent = (!!activity_id_(eventlog) == consequent)*rank) %>%
    ## FILTER FLOW ACTIVITIES
    filter(antecedent > 0 | consequent > 0) %>%
    mutate(antecedent = ifelse(antecedent == 0, NA, antecedent)) %>%
    mutate(consequent = ifelse(consequent == 0, NA, consequent)) %>%
    select(!!resource_id_(eventlog), !!case_id_(eventlog), antecedent, consequent) %>%
    group_by(!!case_id_(eventlog), !!resource_id_(eventlog)) %>%
    summarize(ante = list(antecedent[!is.na(antecedent)]), cons = list(consequent[!is.na(consequent)])) %>%
    ungroup() %>%
    filter(map_int(ante, length) > 0) %>%
    filter(map_int(cons, length) > 0) -> t


  if(precedence_type == "directly_follows") {
    t %>%
      mutate(t = map2(ante, cons, merge)) %>%
      # ARE ANY DIRECTLY OR EVENTUALLY FOLLOWING?
      unnest(t) %>%
      filter(x + 1 == y) %>%
      # DISTINCT TO COMBINE OBSERVATIONS FOR MORE THAN ONE RESOURCe
      distinct(!!case_id_(eventlog))

  } else if(precedence_type == "eventually_follows") {
    t %>%
      mutate(ante = map_int(ante, min)) %>%
      mutate(cons = map_int(cons, max)) %>%
      # ARE ANY DIRECTLY OR EVENTUALLY FOLLOWING?
      filter( ante < cons) %>%
      # DISTINCT TO COMBINE OBSERVATIONS FOR MORE THAN ONE RESOURCe
      distinct(!!case_id_(eventlog))
  }
}



