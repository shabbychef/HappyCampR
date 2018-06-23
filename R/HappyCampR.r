# Copyright 2018-2018 Steven E. Pav. All Rights Reserved.
# Author: Steven E. Pav
#
# This file is part of HappyCampR.
#
# HappyCampR is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# HappyCampR is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with HappyCampR.  If not, see <http://www.gnu.org/licenses/>.

# Created: 2018-06-12
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav
# Comments: Steven E. Pav

#' Shiny App to Find Campgrounds
#' 
#' @section Legal Mumbo Jumbo:
#'
#' HappyCampR is distributed in the hope that it will be useful,
#' but WITHOUT ANY WARRANTY; without even the implied warranty of
#' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#' GNU Lesser General Public License for more details.
#'
#' @template etc
#'
#' @importFrom dplyr mutate arrange select filter rename left_join coalesce distinct summarize everything
#' @importFrom utils data
#' @importFrom shinythemes shinytheme
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addTiles addCircleMarkers
#' @importFrom geosphere distHaversine distGeo
#' @importFrom magrittr %>%
#'
#' @name HappyCampR
#' @rdname HappyCampR
#' @docType package
#' @title Shiny App to Find Campgrounds
#' @keywords package
#' @note
#' 
#' This package is maintained as a hobby. 
#'
NULL


#' @title Campground Data
#' @description ... 
#' @format A \code{data.frame} object with XXX rows and XX columns. The
#' data are from
#'
#' The columns are defined as follows:
#' \describe{
#'  \item{\code{amt}}{The numeric amount of the ingredient.}
#' }
#' @source place, \url{campsite.info}
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @examples
#' \dontrun{
#' data(MoreCamp)
#'
#' }
"MoreCamp"


#' @title News for package 'HappyCampR':
#'
#' @description 
#'
#' News for package \sQuote{HappyCampR}
#'
#' \newcommand{\CRANpkg}{\href{https://cran.r-project.org/package=#1}{\pkg{#1}}}
#' \newcommand{\HappyCampR}{\CRANpkg{HappyCampR}}
#'
#' @section \HappyCampR{} Initial Version 0.1.0 (2018-06-12) :
#' \itemize{
#' \item first CRAN release.
#' }
#'
#' @name HappyCampR-NEWS
#' @rdname NEWS
NULL

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
