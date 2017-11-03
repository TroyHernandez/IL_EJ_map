
library(leaflet)
census_tracts <- readRDS("data/censustracts_IL.Rds")
dat.shiny <- readRDS("data/ShinyDat.RDS")
for(i in 3:ncol(dat.shiny)){
  dat.shiny[, i] <- round(dat.shiny[, i], 2)
}

perc.rank <- function(x) trunc(rank(x)) / length(x)

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
  sidebarLayout(
    sidebarPanel(
      selectInput("col", "Choose a column:",
                  choices = colnames(dat.shiny)[-1],
                  selected = "EnviroScore"),
      plotOutput("hist")),
    mainPanel(leafletOutput("MapPlot1"))
  ),
  fluidRow(DT::dataTableOutput("table"),
           includeMarkdown('README.md'))
)

#Set up server
server <- function(input, output){

  output$table <- DT::renderDataTable(DT::datatable({dat.shiny}))

  output$hist <- renderPlot({
    hist(as.numeric(dat.shiny[, input$col]), main = "", xlab = input$col)
  })
  
  output$MapPlot1 <- renderLeaflet({
    if(input$col == "EJcommunity"){
      pal <- colorNumeric("Reds", NULL,
                           n = length(unique(dat.shiny[, input$col])))
      leaflet.label <- paste0("Tract ", census_tracts$GEOID, " ",
                              input$col, " ",
                              dat.shiny[, input$col])
    } else {
      pal <- colorQuantile("Reds", NULL,
                           n = min(9, length(unique(dat.shiny[, input$col]))))
      leaflet.label <- paste0("Tract ", census_tracts$GEOID, " ",
                              input$col, " ",
                              dat.shiny[, input$col], " EJcommunity ",
                              dat.shiny[, "EJcommunity"])
    }
    leaflet() %>%
      addTiles(urlTemplate = MAPBOX_STYLE_TEMPLATE,
               attribution = mb_attribution) %>%
      addPolygons(data=census_tracts, weight=1, fillOpacity=.5, color="black",
                  fillColor = ~pal(as.numeric(dat.shiny[, input$col])),
                  label=leaflet.label,
                  smoothFactor=.02)
  })
}

#Run app
shinyApp(ui = ui, server = server, options = list(height = 6000))

