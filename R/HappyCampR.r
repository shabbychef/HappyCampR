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
#' @import shiny
#' @importFrom dplyr mutate arrange select filter rename left_join coalesce distinct summarize everything data_frame
#' @importFrom utils data globalVariables
#' @importFrom shinythemes shinytheme
#' @importFrom urltools url_encode 
#' @importFrom leaflet leafletOutput renderLeaflet leaflet addTiles addProviderTiles setView clearMarkers addCircleMarkers
#' @importFrom geosphere distHaversine distGeo
#' @importFrom magrittr %>% %<>%
#' @importFrom lubridate %m+% 
#' @importFrom stringr str_interp
#' @importFrom ggmap geocode  
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
#' @format A \code{data.frame} object with 13,011 rows and over 20 columns,
#' each row respresenting a campground in the US or Canada.
#' The data are taken from USA Campgrounds Info.
#'
#' The columns are defined as follows:
#' \describe{
#'  \item{\code{lon}}{The longitude of the campground, a double roughly
#'  in the range of -160 to -50.}
#'  \item{\code{lat}}{The latitude of the campground, a double roughly in 
#'  the range of 0 to 70.}
#'  \item{\code{campground_code}}{The upstream ID of the campground, a short character
#'  string.}
#'  \item{\code{campground_name}}{The name the campground, a string.}
#'  \item{\code{type}}{The type of the campground, a short string. The types
#'  are encoded as follows: 
#'  \code{NF} for \dQuote{National Forest} (approximately 3800 rows),
#'  \code{CP} for \dQuote{City/County/Regional Park} (around 3000 rows),
#'  \code{SP} for \dQuote{State Park} (around 1700 rows),
#'  \code{COE} for \dQuote{US Corps of Engineers} (around 800 rows),
#'  \code{PP} for \dQuote{Canadian Provincial Park} (around 800 rows),
#'  \code{SF} for \dQuote{State Forest}  (around 700 rows),
#'  \code{BLM} for \dQuote{Bureau of Land Management} (around 400 rows),
#'  \code{NP} for \dQuote{National Park} (around 300 rows),
#'  \code{SFW} for \dQuote{State Fish and Wildlife} (around 250 rows),
#'  \code{SRA} for \dQuote{State Recreation Area} (around 250 rows),
#'  \code{MIL} for \dQuote{Military} (around 200 rows),
#'  \code{UTIL} for \dQuote{Utility} (around 130 rows),
#'  \code{AUTH} for \dQuote{Authority} (around 90 rows),
#'  \code{CNP} for \dQuote{Canadian National Park} (around 80 rows),
#'  \code{SPR} for \dQuote{State Preserve} (around 40 rows),
#'  \code{NRA} for \dQuote{Nanadian National Park} (around 80 rows),
#'  \code{BOR} for \dQuote{Bureau of Reclamation} (around 40 rows),
#'  \code{RES} for \dQuote{Native American Reservation} (around 30 rows),
#'  \code{NM} for \dQuote{National Monument} (around 20 rows),
#'  \code{TVA} for \dQuote{Tennessee Valley Authority} (around 21 rows),
#'  \code{SB} for \dQuote{State Beach} (around 15 rows),
#'  \code{USFW} for \dQuote{US Fish and Wildlife},
#'  \code{AMCW} for \dQuote{US Fish and Wildlife},
#'  a few others, probably miscoded, and
#'  around 130 \code{NA}.}
#'  \item{\code{phone_number}}{The phone number, as a string.}
#'  \item{\code{dates_open}}{The published dates open, as a string.}
#'  \item{\code{comments}}{The published comments, as a string.}
#'  \item{\code{num_campsite}}{The number of campsites, an integer.}
#'  \item{\code{amenities}}{The published amenities, as a string. This is
#'  interpreted as a few other columns that appear later.}
#'  \item{\code{state}}{The US state, or Canadian Province, as a two character
#'  string.}
#'  \item{\code{bearing_to_town}}{The compass bearing to the nearest town.}
#'  \item{\code{nearest_town}}{The nearest town, a string.}
#'  \item{\code{toilets}}{A string describing the type of toilets, one of
#'  \dQuote{vault},
#'  \dQuote{pit},
#'  \dQuote{flush}
#'  \dQuote{none}
#'  \dQuote{flush_and_vault}
#'  or \code{NA}.}
#'  \item{\code{drinking_water}}{Whether drinking water is available, a
#'  boolean.}
#'  \item{\code{reservations}}{Whether reservations can be made at the campground, a
#'  boolean.}
#'  \item{\code{showers}}{Whether showers are available at the campground,
#'  a boolean.}
#'  \item{\code{pets}}{Whether pets are acceptable at the campground,
#'  a boolean.}
#'  \item{\code{opening_week}}{The approximate week number (in the range 1 to
#'  52), when the campground opens, as interpreted from the \code{dates_open}
#'  field.}
#'  \item{\code{closing_week}}{The approximate week number (in the range 1 to
#'  52), when the campground closes, as interpreted from the \code{dates_open}
#'  field.}
#'  \item{\code{elevation_m}}{The elevation of the campground, in meters.}
#'  \item{\code{distance_to_town_km}}{The distance to the nearest town,
#'  in kilometers.}
#' }
#' @source USA Campgrounds Info, compiled by Tom Hillegass,
#' \url{http://www.uscampgrounds.info/}.
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @examples
#' \dontrun{
#' data(MoreCamp)
#'
#' }
"MoreCamp"

globalVariables(c('MoreCamp'))

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
