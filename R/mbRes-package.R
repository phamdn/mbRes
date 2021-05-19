#' mbRes: Integrating Multiple Biomarker Responses using Estimation Statistics
#'
#' Compute and visualize the Biomarker Ordinal Estimator (BOE), a new integrated
#' index for multiple biomarker responses, as described in Pham & Sokolova
#' (2021, unpublished).
#'
#' @author Duy Nghia Pham & Inna M. Sokolova
#'
#' @docType package
#'
#' @name mbRes-package
#'
#' @section Guidelines: BOE is a new integrated index for multiple biomarker
#'   responses. \code{\link{mbr}} is the main function to compute and visualize
#'   the BOE. The others are helper functions and are not meant to be called
#'   directly by users. \code{\link{sokolova2019}} is provided as a sample
#'   dataset.
#'
#' @section Copyright: mbRes: Integrating Multiple Biomarker Responses using
#'   Estimation Statistics. Copyright (C) 2021  Duy Nghia Pham & Inna M.
#'   Sokolova \cr \cr mbRes is free software: you can redistribute it and/or
#'   modify it under the terms of the GNU General Public License as published by
#'   the Free Software Foundation, either version 3 of the License, or (at your
#'   option) any later version. \cr \cr mbRes is distributed in the hope that it
#'   will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
#'   of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#'   General Public License for more details. \cr \cr You should have received a
#'   copy of the GNU General Public License along with mbRes.  If not, see
#'   \url{https://www.gnu.org/licenses/}.
#'
#' @importFrom stats na.omit pnorm qnorm
#' @importFrom magrittr %>% %$%
#' @importFrom dplyr case_when select transmute mutate group_by summarise
#' @importFrom purrr map
#' @importFrom rlang sym
#' @importFrom data.table rbindlist
#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_discrete scale_y_discrete
#'   scale_fill_manual labs theme element_text element_blank
#' @importFrom cowplot theme_cowplot plot_grid get_legend
#' @importFrom grid unit
NULL
