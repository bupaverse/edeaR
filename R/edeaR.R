#' @title edeaR - Exploratory and Descriptive Event-based data Analysis in R
#'
#' @description This package provides several useful techniques for Exploratory and Descriptive analysis of event based data in R, developed by the Business Informatics Research Group of Hasselt University.
#'
#' @docType package
#' @name edeaR
#'
#' @importFrom data.table data.table
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
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
#' @importFrom hms parse_hms
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate wday
#' @importFrom lubridate interval
#' @importFrom glue glue
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats quantile
#' @importFrom stats sd
#' @importFrom stats as.formula
#' @importFrom stats IQR
#' @importFrom stats setNames
#' @importFrom utils head
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @importFrom utils data
#' @import stringr
#' @importFrom rlang expr_text


globalVariables(c(".", ".order","min_order"))
NULL
