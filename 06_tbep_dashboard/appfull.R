# template to app below
# show sidebar vs columns
# leaflet vs leafletproxy

# install.packages('tbeptools', repos = c('https://fawda123.r-universe.dev', 'https://cloud.r-project.org'))
library(tbeptools)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)

datprep <- epcdata %>%
  select(epchc_station, SampleTime, lat = Latitude, lon = Longitude, tn, chla, sd_m) %>%
  pivot_longer(c('tn', 'chla', 'sd_m'), names_to = 'var', values_to = 'val') %>%
  mutate(
    var = factor(var, levels = c('tn', 'chla', 'sd_m'),
                 labels = c('Total Nitrogen (mg/L)', 'Chl-a (ug/L)', 'Secchi depth (m)'))
  )
stas <- unique(datprep$epchc_station)
vars <- unique(datprep$var)

maplocs <- datprep %>%
  select(epchc_station, lat, lon) %>%
  unique()

bsmap <- leaflet(maplocs) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  leaflet::addLabelOnlyMarkers(
    lat = ~lat,
    lng = ~lon,
    label = ~as.character(epchc_station),
    labelOptions = labelOptions(noHide = T, textOnly = T)
  )

ui <- fluidPage(

  titlePanel('Tampa Bay water quality example'),

  column(12,
    column(6, selectInput("stasel", "Select station:", choices = stas)),
    column(6, selectInput("varsel", "Select variable:", choices = vars))
    ),
  column(12,
    plotlyOutput('tsplo'),
    leafletOutput('map1')
  )

)

server <- function(input, output, session) {

  data <- reactive({

    # inputs
    stasel <- input$stasel
    varsel <- input$varsel

    out <- datprep %>%
      filter(epchc_station %in% stasel) %>%
      filter(var %in% varsel)

    return(out)

  })

  output$tsplo <- renderPlotly({

    # inputs
    varsel <- input$varsel
    data <- data()

    p <- ggplot(data, aes(x = SampleTime, y = val)) +
      geom_line() +
      theme_minimal() +
      labs(
        x = NULL,
        y = varsel
      )

    p <- ggplotly(p)

    return(p)

  })

  output$map1 <- renderLeaflet(bsmap)

  observeEvent(input$stasel,{

    # inputs
    stasel <- input$stasel

    staselmap <- maplocs %>%
      filter(epchc_station %in% stasel)

    leafletProxy('map1') %>%
      removeMarker(layerId = 'selected') %>%
      addCircles(
        data = staselmap,
        lat = ~lat,
        lng = ~lon,
        color = 'red',
        weight = 20,
        layerId = 'selected'
      )

  })

}

shinyApp(ui, server)
