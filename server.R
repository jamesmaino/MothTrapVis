library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(ggplot2)
library(plyr)
library(dplyr)

# function used for deciding which traps have missing data for given week
negate_match_df <- function (x, y, on = NULL){
  if (is.null(on)) {
    on <- intersect(names(x), names(y))
  }
  keys <- join.keys(x, y, on)
  x[!(keys$x %in% keys$y), , drop = FALSE]
}

shinyServer(function(input, output, session) {
  zipdata<- reactive({
    if(input$species=='punctigera'){
      return(subset(cleantable,
                    as.Date(yearweek) >= input$dateMin&as.Date(yearweek) <= input$dateMax))  
    }else{
      return(subset(cleantable1,
                    as.Date(yearweek) >= input$dateMin&as.Date(yearweek) <= input$dateMax))
    }
  })
  
  ## Interactive Map ###########################################
  output$yearSlider <- renderUI({
    sliderInput('date', 'Map showing trap data for date range:', min=input$dateMin, max=input$dateMax, value = c(input$dateMin, input$dateMin+input$binSize*7), width = '100%', step = input$binSize*7,animate = animationOptions(interval = input$aniSpeed*1000/as.numeric((input$dateMax-input$dateMin)/(input$binSize*7)),loop=TRUE))
   
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 142, lat = -35, zoom = 6)
  })


  # set up trap data for binned time
  weekdata<- reactive({
    start_date <- input$date[1]
    end_date <- input$date[2]
    swd<-subset(zipdata(), yearweek >=start_date&yearweek<=end_date)
    swd%>%group_by_('id','latitude','longitude',"operator", "state", "district")%>%summarise(count=sum(count, na.rm = TRUE))
    })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  colorBy <- 'count'
  sizeBy <-  'count'
  myIcon =  makeIcon(
    iconUrl = "http://cdn1.iconfinder.com/data/icons/aye-ayecons/32/04-mark-512.png",
    iconWidth = 10, iconHeight = 10)
  
  observe({
    rowsToFind <- weekdata()[,c('longitude','latitude')]
    missing<-negate_match_df(unique(zipdata()[,c('longitude','latitude')]), rowsToFind)
    if (nrow(weekdata())==0){
      leafletProxy("map", data = weekdata()) %>%
        clearShapes() %>% clearMarkers() %>%
        addMarkers(missing$longitude, missing$latitude, icon = myIcon)
    }else{
      colorData <- weekdata()[[colorBy]]
      pal <- colorNumeric('YlOrRd', colorData)
      radius <- log(weekdata()[[sizeBy]]+2) / log(max(weekdata()[[sizeBy]])+2) * 30000
      leafletProxy("map", data = weekdata()) %>%
        clearShapes() %>% clearMarkers() %>%
        addCircles(~longitude, ~latitude, layerId=~id, radius = radius, #radius=6000,
                   stroke = TRUE, color = "black", weight = 1,
                   fillOpacity = ifelse(weekdata()[[sizeBy]]==0,0,0.8),
                   fillColor=pal(colorData)) %>%
        addMarkers(missing$longitude, missing$latitude,icon =  myIcon) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title='Count',
          layerId="colorLegend",opacity = 1)
      
    }
  })


})
