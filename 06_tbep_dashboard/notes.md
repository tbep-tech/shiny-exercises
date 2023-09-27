*Goal*: Create an app to plot a time series and map for a selected monitoring station and parameter

1. Explain shiny template
1. Setup libraries and data prep
     * tbeptools, ggplot2, dplyr, tidyr, plotly, leaflet
     * `datprep`, `stas` (stations), `vars` (variables), `maplocs`, `bsmap`
1. Setup ui inputs
     * `stasel` (station selection), `varsel` (variable selection)
1. Setup server components
     * `data` (selected data from `stasel`, `varsel`) as `reactive()`
     * `tsplo` (input is `data`, `varsel` (for y-axis label)) as `renderPlotly()`
1. Setup ui output
     * `tsplo` using `plotlyOutput()`
1. Setup server components for map
     * `stasel` (input), `bsmap` (global) as inputs, output as `renderLeaflet()`
1. Setup ui output for map
     * `map` using `leafletOutput()`
1. Show `browser()`
1. Change ui layout
1. Show `leafletProxy()`
