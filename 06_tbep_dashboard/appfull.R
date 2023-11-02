# goal: create an app to select a time series by station/parameter, create a plot and show on map

# global.R ----

# * load libraries ----
library(dplyr)
library(ggplot2)
library(leaflet)
library(plotly)
if (!"tbeptools" %in% rownames(installed.packages()))
  install.packages(
    "tbeptools",
    repos = c(
      tbeptech = 'https://tbep-tech.r-universe.dev',
      CRAN     = 'https://cloud.r-project.org') )
library(tbeptools)
library(tidyr)

# * prep data ----
d <- epcdata |>
  select(
    station                 = epchc_station,
    SampleTime,
    lat                     = Latitude,
    lon                     = Longitude,
    `Total Nitrogen (mg/L)` = tn,
    `Chlorophyll-a (ug/L)`  = chla,
    `Secchi depth (m)`      = sd_m) |>
  pivot_longer(
    names_to  = 'indicator',
    values_to = 'value',
    `Total Nitrogen (mg/L)`:`Secchi depth (m)`)

# * data for select ----
stations   <- unique(d$station)
indicators <- unique(d$indicator)
locations  <- d |>
  select(station, lon, lat) |>
  unique()

# * base map ----
basemap <- leaflet(locations) |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addLabelOnlyMarkers(
    lat          = ~lat,
    lng          = ~lon,
    label        = ~as.character(station),
    labelOptions = labelOptions(
      noHide   = T,
      textOnly = T) )

#  ui.R ----
ui <- fluidPage(

  wellPanel(
    h2("Water Quality"),
    selectInput("sel_sta", "Station",   choices = stations),
    selectInput("sel_ind", "Indicator", choices = indicators),
    plotlyOutput('tsplot'),
    leafletOutput('map') )

)

#  server.R ----
server <- function(input, output, session) {

  # * get_data(): reactive to inputs ----
  get_data <- reactive({

    d |>
      filter(
        station   == input$sel_sta,
        indicator == input$sel_ind)
  })

  # * tsplot: time series plot ----
  output$tsplot <- renderPlotly({

    g <- ggplot(
      get_data(),
      aes(
        x = SampleTime,
        y = value) ) +
      geom_line() +
      labs(y = input$sel_ind)

    ggplotly(g)
  })

  # * map ----
  output$map <- renderLeaflet({

    # filter locations by station
    locs_sta <- locations |>
      filter(
        station == input$sel_sta)

    basemap |>
      addCircles(
        data   = locs_sta,
        lng    = ~lon,
        lat    = ~lat,
        color  = 'red',
        weight = 20)
  })
}

# run ----
shinyApp(ui, server)
