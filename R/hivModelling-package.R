#' @name hivModelling
#'
#' @title
#' Evidence-Based Methods to Calculate HIV Incidence in a Given Population
#'
#' @description
#' Calculates HIV incidence in a given population.
#'
#' @author
#' Author: Ard van Sighem \email{a.i.vansighem@@amc.uva.nl}\cr
#' Author: Chantal Quinten \email{chantal.quinten@@ecdc.europa.eu}\cr
#' Creator: Daniel Lewandowski \email{daniel@@nextpagesoft.net}
#'
#' @import data.table
#' @import xml2
#' @import ggplot2
#' @importFrom utils modifyList tail setTxtProgressBar txtProgressBar unzip
#' @importFrom stats setNames rnbinom rbinom rpois quantile
#' @useDynLib hivModelling, .registration = TRUE
#' @importFrom Rcpp sourceCpp
"_PACKAGE"
