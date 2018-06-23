# /usr/bin/r
#
# Created: 2018.06.13
# Copyright: Steven E. Pav, 2018
# Author: Steven E. Pav <steven@gilgamath.com>
# Comments: Steven E. Pav

# nonsense to get CRAN checks and NSE to play nice.
globalVariables(c("campground_name","sdist","type","lat","lon","nearest_town","state","elevation_m","num_campsite","dates_open",
									"campground","nearest town","num campsites","dates open","closing_week","opening_week",
									"toilets","showers","drinking_water","reservations",
									"MoreCamp"))

# define UI logic# FOLDUP
#indat <- readr::read_csv('../intermediate/MoreCamp.csv') 

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

# Define UI 
my_ui <- function(){shinyUI(
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
			h3('Parameters'),
			fluidRow(column(10,
											div(style='vertical-alignment:bottom',
											textInput("location_lookup","Lookup Location:",value='',placeholder='Monument Valley'))),
							 column(2,div(style='vertical-alignment:bottom',actionButton("do_lookup",label='go')))),
			fluidRow(column(6,numericInput("sel_lat","Latitude",value=37.7749,min=0,max=90,step=0.0001)),
							 column(6,numericInput("sel_lon","Longitude",value=-122.4194,min=-180,max=180,step=0.0001))),
			selectInput("sel_type","Campground Type:",choices=types,
									selected=c('NP','SP','NM','SF','NF','BLM','SRA','SPR','SB','SFW','CP','RES'),
									multiple=TRUE),
			fluidRow(column(6,
				selectInput("sel_toilets","Toilets:",choices=c('flush','vault','pit','flush_and_vault','none',NA),
										selected='flush',multiple=TRUE),
				selectInput("sel_showers","Showers:",choices=c('true','false','unknown'),
										selected='true',multiple=TRUE)),
							 column(6,
			selectInput("sel_drinking_water","Drinking Water:",choices=c('true','false','unknown'),
									selected='true',multiple=TRUE),
			selectInput("sel_reservations","Reservations:",choices=c('true','false','unknown'),
									selected='true',multiple=TRUE))),   # end row
			selectInput("sel_units","Units:",choices=c('metric','imperial'),selected='metric',multiple=FALSE),
			hr(),
			sliderInput("sel_elevation","Elevation Range (m)",sep=',',post='',min=-100,max=4000,value=c(0,2500)),
			sliderInput("sel_dist","Distance to point (km)",sep=',',post='',min=0,max=800,value=c(0,80)),
			sliderInput("sel_num_campsite","Num Campsite Range",sep=',',post=' sites',min=0,max=1000,value=c(0,250)),
			helpText('Some campgrounds are closed part of the year.',
							 'If you select a date, and click the checkbox,',
							 'we will restrict by opening and closing date. (experimental)'),
			dateInput("sel_date","Target Date",format='yyyy-mm-dd',
								startview='month',weekstart=1,value=Sys.Date() %m+% months(2)),
			checkboxInput("sel_restrict_date","Restrict by Date?",value=FALSE),
			hr(),
			helpText('data scraped from the web'),
			bookmarkButton('bookmark',title='bookmark page'),
			textOutput('debugging'),
			hr()
			),#UNFOLD
	mainPanel(#FOLDUP
		width=9,
		tabsetPanel(
			tabPanel('data',#FOLDUP
					leaflet::leafletOutput('camps_map',width='100%',height='600px'),
					DT::dataTableOutput('camp_table')
					)#UNFOLD
				) # tabSetPanel
			)  # mainPanel#UNFOLD
		) # sidebarLayout#UNFOLD
	)  # fluidPage#UNFOLD
)}  # shinyUI
# UNFOLD

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

globalVariables(c("MPF","KMPMi"))
MPF <<- 0.3048   # meters per foot
KMPMi <<- 1.60934   # km per mile

# Define server logic # FOLDUP
my_server <- function(input, output, session) {
	viewport <- reactiveValues(lat_lo=0,lat_hi=0,lon_lo=0,lon_hi=0)

	#cat('server!\n',file='/tmp/shiny.err')
	searched <- reactiveValues(text=c(),
														 lat=c(),
														 lon=c())

	observeEvent(input$camps_map_bounds,{
								 viewport$lat_lo <- as.numeric(input$camps_map_bounds[3])
								 viewport$lat_hi <- as.numeric(input$camps_map_bounds[1])
								 viewport$lon_lo <- as.numeric(input$camps_map_bounds[4])
								 viewport$lon_hi <- as.numeric(input$camps_map_bounds[2])

								 lat_cen <- 0.5 * (viewport$lat_lo + viewport$lat_hi)
								 lon_cen <- 0.5 * (viewport$lon_lo + viewport$lon_hi)

								 del_lat <- abs(viewport$lat_hi - viewport$lat_lo)
								 del_lon <- abs(viewport$lon_hi - viewport$lon_lo)

								 # in meters
								 dist_lat <- distHaversine(p1=c(viewport$lon_lo,viewport$lat_lo),
																					 p2=c(viewport$lon_lo,viewport$lat_hi))
								 dist_lon <- distHaversine(p1=c(viewport$lon_lo,viewport$lat_lo),
																					 p2=c(viewport$lon_hi,viewport$lat_lo))
								 # in kilometers
								 min_dist <- 0.5 * 1e-3 * min(dist_lat,dist_lon)
		
								 # nah, don't do this, it screws up the map.
								 # if (!is.null(min_dist)) { updateSliderInput(session,'sel_dist',value=c(min(input$sel_dist),min_dist)) }

								 if (!is.null(lat_cen) && (abs(lat_cen - input$sel_lat) > 0.4*del_lat)) { updateNumericInput(session,'sel_lat',value=lat_cen) }
								 if (!is.null(lon_cen) && (abs(lon_cen - input$sel_lon) > 0.4*del_lon)) { updateNumericInput(session,'sel_lon',value=lon_cen) }
	})


	debuggery <- reactiveValues(msg='')
	output$debugging <- renderText(debuggery$msg)

	observeEvent(input$do_lookup,
					{
						response <- ggmap::geocode(input$location_lookup)
						searched$text <- input$location_lookup
						searched$lat <- response$lat
						searched$lon <- response$lon
						updateNumericInput(session,'sel_lat',value=response$lat)
						updateNumericInput(session,'sel_lon',value=response$lon)
					})

	selunits <- reactiveValues(system='metric')
	observeEvent(input$sel_units,
					{
						maxdist_km <- 800
						minelev_m <- -100
						maxelev_m <- 4000

						old_units <- selunits$system
						new_units <- input$sel_units
						if (old_units != new_units) {
							old_elevation <- input$sel_elevation
							old_dist <- input$sel_dist
							if (new_units=='metric') {
								new_elevation <- old_elevation * MPF
								updateSliderInput(session,'sel_elevation',
																	label="Elevation Range (m)",
																	min=minelev_m,max=maxelev_m,value=round(new_elevation),step=1)

								new_dist <- old_dist * KMPMi 
								updateSliderInput(session,'sel_dist',
																	label="Distance to point (km)",
																	min=0,max=maxdist_km,value=round(new_dist),step=1)
							} else {
								new_elevation <- old_elevation / MPF
								updateSliderInput(session,'sel_elevation',
																	label="Elevation Range (ft)",
																	min=round(minelev_m / MPF),max=round(maxelev_m / MPF),value=round(new_elevation),step=1)

								new_dist <- old_dist / KMPMi 
								updateSliderInput(session,'sel_dist',
																	label="Distance to point (mi)",
																	min=0,max=round(maxdist_km / KMPMi),value=round(new_dist),step=1)
							}
						}
						selunits$system <- new_units
					})

	# in km
	maximum_radius <- reactive({
		units <- input$sel_units
		if (units=='metric') {
			myrad <- input$sel_dist
		} else {
			myrad <- input$sel_dist * KMPMi
		}
		max(myrad)
	})
	just_load <- reactive({
		#indat <- readr::read_csv('../intermediate/MoreCamp.csv') 
		#indat <- readr::read_csv('campdata/intermediate/MoreCamp.csv') 
		data("MoreCamp", package="HappyCampR")
		indat <- MoreCamp
		
		indat
	})

	filtered_data <- reactive({
		indat <- just_load()
		if (input$sel_units=='metric') {
			elrange <- input$sel_elevation
		} else {
			elrange <- input$sel_elevation * MPF
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
		srch_df <- data_frame(lon=input$sel_lon,
													lat=input$sel_lat,
													location=coalesce(input$location_lookup,''))
	})


	dist_data <- reactive({
		srch_lonlat <- c(input$sel_lon,input$sel_lat)

		if (input$sel_units=='metric') {
			dirange <- input$sel_dist
		} else {
			dirange <- input$sel_dist * KMPMi
		}

		otdat <- filtered_data() %>%
			mutate(sdist = round(1e-3 * geosphere::distGeo(srch_lonlat,matrix(c(lon,lat),ncol=2) ),digits=2)) %>%
			filter(sdist >= min(dirange),sdist <= max(dirange)) %>%
			arrange(sdist) 
		otdat 
	})

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
				mutate(elevation_m=elevation_m / MPF,
							 sdist=sdist / KMPMi) %>%
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

	output$camps_map <- leaflet::renderLeaflet({
		otdat <- dist_data()
		srch_df <- search_data()

		ph <- leaflet::leaflet(otdat) %>%
				leaflet::addTiles() %>%
				leaflet::addCircleMarkers(lat=~lat,lng=~lon,popup=~campground_name,
																	color="#03F") %>%
				leaflet::addCircleMarkers(lat=srch_df$lat,lng=srch_df$lon,popup='searched point',
																	color="#F30")

		ph
	})

	observeEvent(input$camps_map_zoom,{
								 debuggery$msg <- paste('zoom changed to',input$camps_map_zoom)
	})

	setBookmarkExclude(c('bookmark'))
	observeEvent(input$bookmark,{
								 session$doBookmark()
	})

}
# UNFOLD

#' @title campr_app .
#'
#' @description 
#'
#' One sentence or so that tells you some more.
#'
#' @details
#'
#' Really detailed. \eqn{\zeta}{zeta}.
#'
#' A list:
#' \itemize{
#' \item I use \eqn{n}{n} to stand for blah.
#' \item and so forth....
#' }
#'
#' @usage
#'
#' campr_app()
#'
#' @return Runs the shiny app.
#'
#' @keywords shiny
#' @template etc
#' @examples 
#' \dontrun{
#' campr_app()
#' }
#' @author Steven E. Pav \email{steven@@gilgamath.com}
#' @export
campr_app <- function() {
	shinyApp(ui=my_ui(), server=my_server)
}

#for vim modeline: (do not edit)
# vim:fdm=marker:fmr=FOLDUP,UNFOLD:cms=#%s:syn=r:ft=r
