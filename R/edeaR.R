#' @title edeaR - Exploratory and Descriptive Event-based data Analysis in R
#'
#' @description This package provides several useful techniques for Exploratory and Descriptive analysis of event based data in R, developed by the Business Informatics Research Group of Hasselt University.
#'
#' @docType package
#' @name edeaR
#'
#' @keywords internal
"_PACKAGE"
#'
## usethis namespace: start
#' @import bupaR
#' @import dplyr
#' @import ggplot2
#' @import ggthemes
#' @import tibble
#' @import shiny
#' @import miniUI
#' @import tidyr
#' @import shinyTime
#' @import purrr
#' @import zoo
#' @import stringr
#' @importFrom hms parse_hms
#' @importFrom lubridate ymd_hms wday interval year hour hours minute minutes second seconds make_date
#' @importFrom glue glue
#' @importFrom stats median na.omit quantile sd as.formula IQR setNames
#' @importFrom utils head setTxtProgressBar txtProgressBar data
#' @importFrom data.table data.table := as.data.table
#' @importFrom rlang expr_text maybe_missing arg_match caller_env
#' @importFrom lifecycle deprecated
#' @importFrom cli cli_warn cli_abort
## usethis namespace: end

globalVariables(c(".", ".order","min_order", "n"))
NULL
