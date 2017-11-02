
library(leaflet)
census_tracts <- readRDS("data/censustracts_IL.Rds")
dat.shiny <- readRDS("data/ShinyDat.RDS")
dat.shiny$EnviroScore <- round(dat.shiny$EnviroScore, 2)
dat.shiny$EnvironmentalIndicator <- round(dat.shiny$EnvironmentalIndicator, 2)
dat.shiny$DemographicIndicator <- round(dat.shiny$DemographicIndicator, 2)

perc.rank <- function(x) trunc(rank(x)) / length(x)
pal <- colorQuantile("Reds", NULL, n = 9)

##==============================================================================
## MAPBOX KEY
## Register at mapbox.com and create a map... or use the one I made
##==============================================================================
MAPBOX_STYLE_TEMPLATE <- paste0("https://api.mapbox.com/styles/v1/coc375492/",
                                "cirqd7mgf001ygcnombg4jtb4/tiles/256/{z}/{x}/{y}",
                                "?access_token=pk.eyJ1IjoiY29jMzc1NDkyIiwiYSI6ImN",
                                "pcnBldzVqMTBmc3J0N25rZTIxZ3ludDIifQ.DgJIcLDjC1h9MtT8CaJ-pQ")
mb_attribution <- paste("© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> ",
                        "© <a href='http://www.openstreetmap.org/about/'>OpenStreetMap</a>")

##==============================================================================
## DEFINE UI AND SERVER COMPONENTS
##==============================================================================

# https://github.com/SimonGoring/ShinyLeaflet-tutorial/blob/master/Shiny-leaflet-tutorial.Rmd

ui <- fluidPage(
  leafletOutput("MapPlot1"),
  fluidRow(DT::dataTableOutput("table"), height = 600, width = 800)
)

#Set up server
server <- function(input, output){

  output$table <- DT::renderDataTable(DT::datatable({dat.shiny}))

  output$MapPlot1 <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE, attribution = mb_attribution) %>%
      addPolygons(data=census_tracts, weight=1, fillOpacity=.5, color="black",
                  fillColor = ~pal(dat.shiny$EnviroScore),
                  label=paste0("Census Tract ", census_tracts$GEOID, "\n",
                               "EnviroScore Percentile ",
                               round(perc.rank(dat.shiny$EnviroScore), 2)),
                  smoothFactor=.02)
  })
}

#Run app
shinyApp(ui = ui, server = server, options = list(height = 6000))

