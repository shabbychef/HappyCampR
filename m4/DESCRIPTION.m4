dnl divert here just means the output from basedefs does not appear.
divert(-1)
include(basedefs.m4)
divert(0)dnl
Package: PKG_NAME()
Maintainer: Steven E. Pav <shabbychef@gmail.com>
Authors@R: c(person(c("Steven", "E."), "Pav", 
    role=c("aut","cre"),
    email="shabbychef@gmail.com",
    comment = c(ORCID = "0000-0002-4197-6195")))
Version: VERSION()
Date: DATE()
License: LGPL-3
Title: Shiny App to Find Campgrounds
BugReports: https://github.com/shabbychef/PKG_NAME()/issues
Description: A Shiny app to find campgrounds.
Depends: 
    R (>= 3.0.2),
    shiny
Imports:
    dplyr,
    ggplot2,
    DT,
    leaflet,
    shinythemes,
dnl readr,
    ggmap,
    geosphere,
    magrittr,
    urltools,
    stringr,
    lubridate
URL: https://github.com/shabbychef/PKG_NAME()
dnl VignetteBuilder: knitr
Collate:
m4_R_FILES()
dnl vim:ts=2:sw=2:tw=79:syn=m4:ft=m4:et
