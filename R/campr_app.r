# /usr/bin/r
#
# Created: 2018.06.13
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav <steven@gilgamath.com>
# Comments: Steven E. Pav

.carp <- function(...) { cat(...,file='~/camp_app.log',append=TRUE) }

# nonsense to get CRAN checks and NSE to play nice.
utils::globalVariables(c("campground_name","sdist","type","lat","lon","nearest_town","state","elevation_m","num_campsite","dates_open",
									"campground","nearest town","num campsites","dates open","closing_week","opening_week",
									"toilets","showers","drinking_water","reservations",
									"MoreCamp"))

# some utilities# FOLDUP
.applylink <- function(title,url) {
	as.character(a(title,href=url,target="_blank"))
}
applylink <- function(title,url) {
	as.character(mapply(.applylink,title,url))
}
.search_link <- function(campground,city,state) {
	searchterm <- urltools::url_encode(paste(campground,city,state))
	url <- stringr::str_interp('https://www.google.com/search?q=${searchterm}&ie=utf-8&oe=utf-8')
	.applylink(title=campground,url=url)
}
search_link <- function(campground,city,state) {
	as.character(mapply(.search_link,campground,city,state))
}
.logical_it <- function(x) {
	as.logical(toupper(x))
}

utils::globalVariables(c("ERAD_KM","MPF","KMPMi"))

# the radius of the earth, in KM.
ERAD_KM <<- 6378.137 

MPF <<- 0.3048                # meters per foot
KMPMi <<- 5280 * MPF / 1000   # km per mile

.m2ft <- function(x) { x / MPF }
.ft2m <- function(x) { x * MPF }
.km2mi <- function(x) { x / KMPMi }
.mi2km <- function(x) { x * KMPMi }

# UNFOLD

#' @title campr_app .
#'
#' @description 
#'
#' A shiny app to locate campgrounds.
#'
#' @param init_lat  an initial latitude to seed the search.
#' @param init_lon  an initial longitude to seed the search. The
#' default values are for San Francisco.
#' @param init_search an initial string to put into the search text.
#'
#' @return a shiny app.
#'
#' @keywords shiny
#' @template etc
#' @examples 
#' \dontrun{
#' campr_app()
#' }
#' @author Steven E. Pav \email{shabbychef@@gmail.com}
#' @export
campr_app <- function(init_lat=37.7749,init_lon=-122.4194,init_search='San Francisco') {
	if (missing(init_lat) && missing(init_lon) && !missing(init_search) && nzchar(init_search)) {
		tryCatch({
			response <- ggmap::geocode(init_search)
			init_lat <- response$lat
			init_lon <- response$lon
		},error=function(e) {
			init_lat <- 37.7749
			init_lon <- -122.4194
			init_search <- ''
			print(e)
		})
	} 
	# some sensible ranges
	maxdist_km <- 800
	minelev_m <- -100
	maxelev_m <- 4000

	types <- list(national=c('National Park'='NP',
													 'National Monument'='NM',
													 'Canadian National Park'='CNP'),
								forest=c('National Forest'='NF'),
								managed=c('Bureau of Land Management'='BLM',
													'US Fish and Wildlife'='USFW',
													'Bureau of Reclamation'='BOR'),
								etc=c('US Corps of Engineers'='COE','Tennessee Valley Authority'='TVA'),
								state=c('State Park'='SP','Canadian Provincial Park'='PP',
												'State Rec. Area'='SRA','State Preserve'='SPR',
												'State Beach'='SB'),
								state_forest=c('State Forest'='SF',
															 'State Fish and Wildlife'='SFW'),
								military=c('Military only'='ML'),
								other=c('County/City/Regional Park'='CP',
												'Authority'='AUTH',
												'Utility'='UTIL',
												'Native American Reservation'='RES',
												'Unknown'='UNKN'))

	# define UI logic
	my_ui <- shinyUI(
		fluidPage(theme=shinythemes::shinytheme("spacelab"),#FOLDUP
			# for this, see: http://stackoverflow.com/a/22886762/164611
			# Application title
			tags$head(
						# load accounting js
						#tags$script(src='js/accounting.js'),
						tags$script(src='test.js'),
						# points for style:
						tags$style(".table .alignRight {color: black; text-align:right;}"),
						tags$link(rel="stylesheet", type="text/css", href="style.css")
			),
			titlePanel("Happy CampR"),
			# tags$img(id = "logoimg", src = "logo.png", width = "200px"),
			sidebarLayout(#FOLDUP
				position="left",
			sidebarPanel(#FOLDUP
				width=2,
				checkboxInput("follow_map","Link to map",value=FALSE),
				conditionalPanel(condition="input.follow_map == 0",
					fluidRow(column(10,
													div(style='vertical-alignment:bottom',
													textInput("location_lookup","Lookup Location:",value=init_search,placeholder=init_search))),
									 column(2,div(style='vertical-alignment:bottom',actionButton("do_lookup",label='go')))),
					fluidRow(column(6,numericInput("sel_lat","Latitude",value=init_lat,min=0,max=90,step=0.0001)),
									 column(6,numericInput("sel_lon","Longitude",value=init_lon,min=-180,max=180,step=0.0001)))
					),
				fluidRow(column(6,
					selectInput("sel_toilets","Toilets:",choices=c('flush','vault','pit','flush_and_vault','none','unknown'),
											selected='flush',multiple=TRUE),
					selectInput("sel_showers","Showers:",choices=c('true','false','unknown'),
											selected='true',multiple=TRUE)),
								 column(6,
				selectInput("sel_drinking_water","Water:",choices=c('true','false','unknown'),
										selected='true',multiple=TRUE),
				selectInput("sel_reservations","Reservations:",choices=c('true','false','unknown'),
										selected='true',multiple=TRUE))),   # end row
				selectInput("sel_units","Units:",choices=c('metric','imperial'),selected='metric',multiple=FALSE),
				hr(),
				sliderInput("sel_dist","Distance to point (km)",sep=',',post='',min=0,max=maxdist_km,value=c(0,150)),
				sliderInput("sel_elevation","Elevation Range (m)",sep=',',post='',min=minelev_m,max=maxelev_m,value=c(0,3000)),
				helpText('Some campgrounds are closed part of the year.',
								 'If you select a date, and click the checkbox,',
								 'we will restrict by opening and closing date. (experimental)'),
				dateInput("sel_date","Target Date",format='yyyy-mm-dd',
									startview='month',weekstart=1,value=Sys.Date() %m+% months(2)),
				checkboxInput("sel_restrict_date","Restrict by Date?",value=FALSE),
				sliderInput("sel_num_campsite","Num Campsite Range",sep=',',post=' sites',min=0,max=1000,value=c(0,500)),
				selectInput("sel_type","Campground Type:",choices=types,
										selected=c('NP','SP','CNP','AUTH','UNKN','NM','SF','NF','BLM','SRA','SPR','SB','SFW','CP','PP','USFW','BLM','BOR','RES'),
										multiple=TRUE),
				hr(),
				bookmarkButton('bookmark',title='bookmark page'),
				textOutput('debugging'),
				hr()
				),#UNFOLD
		mainPanel(#FOLDUP
			width=9,
			leaflet::leafletOutput('camps_map',width='100%',height='600px'),
			DT::dataTableOutput('camp_table')
				)  # mainPanel#UNFOLD
			) # sidebarLayout#UNFOLD
		)  # fluidPage#UNFOLD
	)  # shinyUI

# Define server logic # FOLDUP
	my_server <- function(input, output, session) {


		viewport <- reactiveValues(lat_cen=init_lat,
															 lon_cen=init_lon,
															 lat_lo=init_lat - 0.2,lat_hi=init_lat + 0.2,
															 lon_lo=init_lon - 0.2,lon_hi=init_lon + 0.2,
															 big_move=1,
															 zoom_level=8)
		centerloc <- reactiveValues(lat_cen=init_lat,
																lon_cen=init_lon)

		searched <- reactiveValues(text=init_search)

		observeEvent({
			input$camps_map_bounds
			input$follow_map
		},{
			#debuggery$msg <- paste('map bounds is',paste(input$camps_map_bounds,collapse='::'))

			if (input$follow_map) {
				viewport$lat_lo <- as.numeric(input$camps_map_bounds[3])
				viewport$lat_hi <- as.numeric(input$camps_map_bounds[1])
				viewport$lon_lo <- as.numeric(input$camps_map_bounds[4])
				viewport$lon_hi <- as.numeric(input$camps_map_bounds[2])

				lat_cen <- 0.5 * (viewport$lat_lo + viewport$lat_hi)
				lon_cen <- 0.5 * (viewport$lon_lo + viewport$lon_hi)

				del_lat <- abs(viewport$lat_hi - viewport$lat_lo)
				del_lon <- abs(viewport$lon_hi - viewport$lon_lo)

			## in meters
				#dist_lat <- distHaversine(p1=c(viewport$lon_lo,viewport$lat_lo),
														 #p2=c(viewport$lon_lo,viewport$lat_hi))
				#dist_lon <- distHaversine(p1=c(viewport$lon_lo,viewport$lat_lo),
														 #p2=c(viewport$lon_hi,viewport$lat_lo))
			## in kilometers
				#min_dist <- 0.5 * 1e-3 * min(dist_lat,dist_lon)

			# nah, don't do this, it screws up the map.
			# if (!is.null(min_dist)) { updateSliderInput(session,'sel_dist',value=c(min(input$sel_dist),min_dist)) }

				if (!is.null(lat_cen) && (abs(lat_cen - centerloc$lat_cen) > 0.35*del_lat)) { 
					centerloc$lat_cen <- input$sel_lat
					updateNumericInput(session,'sel_lat',value=lat_cen)
					updateTextInput(session,'location_lookup',value='',placeholder=searched$text)
				}
				if (!is.null(lon_cen) && (abs(lon_cen - centerloc$lon_cen) > 0.35*del_lon)) { 
					centerloc$lat_cen <- input$sel_lat
					updateNumericInput(session,'sel_lon',value=lon_cen)
					updateTextInput(session,'location_lookup',value='',placeholder=searched$text)
				}
			}
		})

		observeEvent({
			input$sel_lat
		},{
			centerloc$lat_cen <- input$sel_lat
		})
		observeEvent({
			input$sel_lon
		},{
			centerloc$lon_cen <- input$sel_lon
		})

		observeEvent(input$do_lookup,
		{
			response <- ggmap::geocode(input$location_lookup)
			searched$text <- input$location_lookup
			viewport$lat_cen <- response$lat
			viewport$lon_cen <- response$lon
			updateNumericInput(session,'sel_lat',value=response$lat)
			updateNumericInput(session,'sel_lon',value=response$lon)
			viewport$big_move <- viewport$big_move + 1
		})

		# change between metric and imperial# FOLDUP
		selunits <- reactiveValues(system='metric')
		observeEvent(input$sel_units,
		{
			old_units <- selunits$system
			new_units <- input$sel_units
			if (old_units != new_units) {
				old_elevation <- input$sel_elevation
				old_dist <- input$sel_dist
				if (new_units=='metric') {
					new_elevation <- .ft2m(old_elevation)
					updateSliderInput(session,'sel_elevation',
														label="Elevation Range (m)",
														min=minelev_m,max=maxelev_m,value=round(new_elevation),step=1)

					new_dist <- .mi2km(old_dist)
					updateSliderInput(session,'sel_dist',
														label="Distance to point (km)",
														min=0,max=maxdist_km,value=round(new_dist),step=1)
				} else {
					new_elevation <- .m2ft(old_elevation)
					updateSliderInput(session,'sel_elevation',
														label="Elevation Range (ft)",
														min=round(.m2ft(minelev_m)),max=round(.m2ft(maxelev_m)),value=round(new_elevation),step=1)

					new_dist <- .km2mi(old_dist)
					updateSliderInput(session,'sel_dist',
														label="Distance to point (mi)",
														min=0,max=round(.km2mi(maxdist_km)),value=round(new_dist),step=1)
				}
			}
			selunits$system <- new_units
		})# UNFOLD

		# get the data.
		just_load <- reactive({
			utils::data("MoreCamp", package="HappyCampR")
			indat <- MoreCamp %>%
				mutate(toilets=ifelse(is.na(toilets),'unknown',toilets))
			indat
		})

		# filter campgrounds# FOLDUP
		filtered_data <- reactive({
			indat <- just_load()
			# based on elevation.
			if (input$sel_units=='metric') {
				elrange <- input$sel_elevation
			} else {
				elrange <- .ft2m(input$sel_elevation)
			}

			otdat <- indat %>%
				filter((length(input$sel_type)==0) | (type %in% input$sel_type),
							 (length(input$sel_toilets)==0) | (toilets %in% input$sel_toilets),
							 (length(input$sel_showers)==0) | (showers %in% .logical_it(input$sel_showers)),
							 (length(input$sel_drinking_water)==0) | (drinking_water %in% .logical_it(input$sel_drinking_water)),
							 (length(input$sel_reservations)==0) | (reservations %in% .logical_it(input$sel_reservations)),
							 (!is.na(num_campsite) & (num_campsite >= min(input$sel_num_campsite) & num_campsite <= max(input$sel_num_campsite)) | (is.na(num_campsite))),
							 (!is.na(elevation_m) & (elevation_m >= min(elrange)) & (elevation_m <= max(elrange))) | (is.na(elevation_m)))

			if (input$sel_restrict_date) {
				weeknum <- lubridate::isoweek(input$sel_date)
				# get rid of things which are definitely closed.
				otdat %<>%
					filter((is.na(opening_week) | is.na(closing_week)) |
								 ((opening_week <= weeknum) & (weeknum <= closing_week)) |   # closed in winter, which makes sense
								 ((closing_week < opening_week) & ((opening_week <= weeknum) | (weeknum <= closing_week))))
			}

			otdat
		})


		search_data <- reactive({
			srch_lonlat <- c(centerloc$lon_cen,centerloc$lat_cen)
			srch_df <- data_frame(lon=srch_lonlat[1],lat=srch_lonlat[2],
														location=ifelse(is.null(input$location_lookup),'',
																						coalesce(input$location_lookup,'')))
		})

		# attach distance to the latitude and longitude point.
		dist_data <- reactive({
			srch_lonlat <- c(centerloc$lon_cen,centerloc$lat_cen)

			if (input$sel_units=='metric') {
				dirange <- input$sel_dist
			} else {
				dirange <- .mi2km(input$sel_dist)
			}

			otdat <- filtered_data() 
			if (nrow(otdat) > 0) {
				otdat %<>%
					mutate(sdist = round(1e-3 * geosphere::distGeo(srch_lonlat,matrix(c(lon,lat),ncol=2) ),digits=2)) %>%
					filter(sdist >= min(dirange),sdist <= max(dirange)) %>%
					arrange(sdist) 
			} else {
				# is that right?
				otdat$sdist <- NULL
			}
			otdat 
		})# UNFOLD

		# table of comparables #FOLDUP
		output$camp_table <- DT::renderDataTable({
			otdat <- dist_data()

			showdat <- otdat %>%
				select(campground_name,sdist,
							 type,lat,lon,
							 nearest_town,state,
							 elevation_m,
							 num_campsite,
							 drinking_water,toilets,showers,reservations,dates_open)  %>%
			rename(`campground`=campground_name,
						 `nearest town`=nearest_town,
						 `num campsites`=num_campsite,
						 `dates open`=dates_open)

			if (selunits$system=='imperial') {
				showdat %<>%
					mutate(elevation_m=.m2ft(elevation_m),
								 sdist=.km2mi(sdist)) %>%
					mutate(elevation_m=round(elevation_m),
								 sdist=round(sdist,1)) %>%
					rename(`dist to point, mi`=sdist,
								 `elevation ft`=elevation_m)
			} else {
				showdat %<>%
					mutate(elevation_m=round(elevation_m),
								 sdist=round(sdist,1)) %>%
					rename(`dist to point, km`=sdist,
								 `elevation m`=elevation_m)
			}

			# render NA logicals as unknown
			showdat %<>%
				mutate(showers=ifelse(is.na(showers),'unknown',as.character(showers)),
							 reservations=ifelse(is.na(reservations),'unknown',as.character(reservations)))

			showdat %<>%
				mutate(campground=search_link(campground,`nearest town`,state))

			# for this javascript shiznit, recall that javascript starts
			# counting at zero!
			#
			# cf 
			# col rendering: http://rstudio.github.io/DT/options.html
			# https://github.com/jcheng5/shiny-jsdemo/blob/master/ui.r
			DT::datatable(showdat,
										caption='Matching campgrounds',
										escape=FALSE,
										rownames=FALSE,
										options=list(order=list(list(1,'asc')),
																 paging=TRUE,
																 pageLength=15))
		},
		server=TRUE)#UNFOLD

		# render the leaflet map
		output$camps_map <- leaflet::renderLeaflet({
			viewport$big_move 
			isolate({
				#leaflet::addProviderTiles(providers$OpenStreetMap) %>%
				ph <- leaflet::leaflet() %>%
					leaflet::addTiles() %>%
					leaflet::setView(lat=viewport$lat_cen,lng=viewport$lon_cen,zoom=viewport$zoom_level)
			})
			ph
		})

		observe({
			otdat <- dist_data() %>%
				mutate(color="#03F")
			# when rows are selected, change their color on the map. 
			if (length(input$camp_table_rows_selected)) {
				otdat[input$camp_table_rows_selected,]$color <- "#1D5"
			}

			srch_df <- search_data()
			leaflet::leafletProxy("camps_map",data=otdat) %>%
				leaflet::clearMarkers() %>%
				leaflet::addCircleMarkers(lat=~lat,lng=~lon,popup=~campground_name,
																	color=~color) %>%
				leaflet::addCircleMarkers(lat=srch_df$lat,lng=srch_df$lon,popup='searched point',
																	color="#F30")
		})

		observeEvent(input$camps_map_zoom,{
			viewport$zoom_level <- input$camps_map_zoom
			debuggery$msg <- paste('zoom changed to',input$camps_map_zoom)
		})
		debuggery <- reactiveValues(msg='')
		output$debugging <- renderText(debuggery$msg)


		setBookmarkExclude(c('bookmark'))
		observeEvent(input$bookmark,{
									 session$doBookmark()
		})

	}
	# UNFOLD

	shinyApp(ui=my_ui, server=my_server)
}

options(shiny.reactlog=TRUE)

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
