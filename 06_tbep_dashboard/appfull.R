# goal: create an app to select a time series by station/parameter, create a plot and show on map

# setup
library(tbeptools)
library(dplyr)
library(tidyr)
library(leaflet)
library(plotly)
library(ggplot2)

# prep data
datprep <- epcdata %>%
  select(
    epchc_station,
    SampleTime,
    lat = Latitude,
    lon = Longitude,
    `Total Nitrogen (mg/L)` = tn,
    `Chlorophyll-a (ug/L)` = chla,
    `Secchi depth (m)` = sd_m
  ) %>%
  pivot_longer(names_to = 'var', values_to = 'val', `Total Nitrogen (mg/L)`:`Secchi depth (m)`)

# station selection
stasel <- unique(datprep$epchc_station)

# parameter selection
varsel <- unique(datprep$var)

# map locations
maplocs <- datprep %>%
  select(epchc_station, lon, lat) %>%
  unique()

# base map
bsmap <- leaflet(maplocs) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLabelOnlyMarkers(
    lat = ~lat,
    lng = ~lon,
    label = ~as.character(epchc_station),
    labelOptions = labelOptions(noHide = T, textOnly = T)
  )

#  create vector of lat/lon
ui <- fluidPage(

  wellPanel(
    h2("Plot water quality data"),
    selectInput("stasel", "Select station", choices = stasel),
    selectInput("varsel", "Select parameter", choices = varsel),
    plotlyOutput('tsplo'),
    leafletOutput('map1')
  )

)

server <- function(input, output, session) {

  # setup plot data as reactive
  data <- reactive({

    # inputs
    stasel <- input$stasel
    varsel <- input$varsel

    out <- datprep %>%
      filter(epchc_station == stasel) %>%
      filter(var == varsel)

    return(out)

  })

  # setup time series plot for output
  output$tsplo <- renderPlotly({

    # inputs
    data <- data()
    varsel <- input$varsel

    p <- ggplot(data, aes(x = SampleTime, y = val)) +
      geom_line() +
      labs(
        y = varsel
      )

    p <- ggplotly(p)

    return(p)

  })

  # setup map for output
  output$map1 <- renderLeaflet({

    # inputs
    stasel <- input$stasel

    # filter lat/lon by station
    maplocsel <- maplocs %>%
      filter(epchc_station == stasel)

    m <- bsmap %>%
      addCircles(
        data = maplocsel,
        lng = ~lon,
        lat = ~lat,
        color = 'red',
        weight = 20
      )

    return(m)

  })

}

shinyApp(ui, server)
