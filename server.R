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
    if (!is.null(input$map_bounds)){
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
    }else{
      latRng <- c(-90,90)
      lngRng  <- c(-180,180)
    }
    if(input$species=='punctigera'){
      return(subset(cleantable,
                    as.Date(date) >= input$dateMin&as.Date(date) <= input$dateMax &
                      latitude >= latRng[1] & latitude <= latRng[2] &
                      longitude >= lngRng[1] & longitude <= lngRng[2]))  
    }else{
      return(subset(cleantable1,
                    as.Date(date) >= input$dateMin&as.Date(date) <= input$dateMax&
                      latitude >= latRng[1] & latitude <= latRng[2] &
                      longitude >= lngRng[1] & longitude <= lngRng[2]))
    }
  })
  
  ## Interactive Map ###########################################
  output$yearSlider <- renderUI({
    input$refresh
    sliderInput('date', 'Map showing trap data for date range:', min=input$dateMin, max=input$dateMax, value = c(input$dateMin, input$dateMin+input$binSize*7), width = '100%', step = input$binSize*7,animate = animationOptions(interval = input$aniSpeed*1000/as.numeric((input$dateMax-input$dateMin)/(input$binSize*7)),loop=FALSE))
   
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = 142, lat = -35, zoom = 6)
  })
  # set max of the legend for animation (so doesn't jump around)
  legendMax<-reactive({
    df <- subset(zipdata(), date >=input$dateMin&date<=input$dateMax )
    df$variablebin<-as.Date(NA, origin = "1970-1-1")
    datebins<-seq(input$dateMin,input$dateMax, by = 7*ifelse(is.na(input$binSize),1,input$binSize))
    for(datebin in datebins){
      df$variablebin[which(0<=(df$date-datebin)&(df$date-datebin)<=7*input$binSize)]<-as.Date(datebin, origin = '1970-1-1')
    }
    count<-df%>%group_by_('id','variablebin')%>%summarise(count=sum(count, na.rm = TRUE))
    return(max(count$count))
  })
  # set up trap data for binned time
  weekdata<- reactive({
    start_date <- input$date[1]
    end_date <- input$date[2]
    swd<-subset(zipdata(), date >=start_date&date<=end_date)
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
      colorData <- c(weekdata()[[colorBy]])
      legMax<-legendMax()
      pal <- colorNumeric('YlOrRd', c(legMax,colorData))
      radius <- 6^3/input$map_zoom^3*log(weekdata()[[sizeBy]]+2) / log(max(legMax)+2) * 30000
      leafletProxy("map", data = weekdata()) %>%
        clearShapes() %>% clearMarkers() %>%
        addCircles(~longitude, ~latitude, layerId=~id, radius = radius, #radius=6000,
                   stroke = TRUE, color = "black", weight = 1,
                   fillOpacity = ifelse(weekdata()[[sizeBy]]==0,0,1),
                   fillColor=pal(colorData)) %>%
        addMarkers(missing$longitude, missing$latitude,icon =  myIcon) %>%
        addLegend("right", pal=pal, values=c(colorData, legMax), title='Count',
          layerId="colorLegend",opacity = 1)
      
    }
  })
  
  

})
