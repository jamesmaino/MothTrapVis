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
  ## Interactive Map ###########################################
  zipdata<- reactive({
    if(input$species=='punctigera'){
      return(subset(cleantable,
                    as.numeric(format(as.Date(yearweek), '%Y')) == format(WA_set$Start_date, format='%Y')))  
    }else{
      return(subset(cleantable1,
                    as.numeric(format(as.Date(yearweek), '%Y')) == format(WA_set$Start_date, format='%Y')))
    }
  })
  animationOptions(interval = 1000, loop = FALSE, playButton = 'p')
  output$yearSlider <- renderUI({
    subct <- subset(zipdata(), format(zipdata()$yearweek, '%Y') == format(WA_set$Start_date, format='%Y'))
    # if(is.null(input$date)){
    #   
    #   myDates <- c(as.Date(paste0(format(WA_set$Start_date, format='%Y'),'-08-20')),as.Date(paste0(format(WA_set$Start_date, format='%Y'),'-08-27')))
    #   }else{
    #     myDates<-input$date
    #   }
    myMin<-min(as.Date(subct$yearweek))
    myMax<-max(as.Date(subct$yearweek))+ 7 
    myMax<- myMax - (as.numeric(myMax)-as.numeric(myMin))%%7 # substract remainder to ensure max is divisible by 7
    mySpan<-myMax-myMin
    sliderInput('date', 'Map showing trap data for date range:',min=myMin, max=myMax, value = c(myMin,myMin+7), width = '100%', step = 7)
    
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=b44e15e05f4d4aab955704c3e6f57f2f",
        attribution = 'Maps <a href="http://www.thunderforest.com/">© Thunderforest</a>, Data <a href="http://www.openstreetmap.org/copyright">© OpenStreetMap contributors</a> '
      ) %>%
      setView(lng = WA_set$Initial_longitude, lat = WA_set$Initial_latitude, zoom = 6)
  })
  
  # set up trap data for binned time
  
  
  weekdata<- reactive({
    start_date <- input$date[1]
    end_date <- input$date[2]
    swd<-subset(zipdata(), yearweek >=start_date&yearweek<=end_date)
    swd%>%group_by_('id','latitude','longitude',"operator", "state", "location")%>%summarise(count=sum(count, na.rm = TRUE))
  })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  colorBy <- 'count'
  sizeBy <-  'count'
  myIcon =  makeIcon(
    iconUrl = "http://cdn1.iconfinder.com/data/icons/aye-ayecons/32/04-mark-512.png",
    iconWidth = 10, iconHeight = 10)
  myMoth =  makeIcon(
    iconUrl = "no_moth.png",
    iconWidth = 30, iconHeight = 30)
  
  observe({
    rowsToFind <- weekdata()[,c('longitude','latitude')]
    missing<-negate_match_df(unique(zipdata()[,c('longitude','latitude')]), rowsToFind)
    if (nrow(weekdata())==0){
      leafletProxy("map", data = weekdata()) %>%
        clearShapes() %>% clearMarkers() %>%
        addMarkers(missing$longitude, missing$latitude, popup = 'No data at selected week',icon = myIcon)
    }else{
      colorData <- weekdata()[[colorBy]]
      zeros=subset( weekdata(), count ==0)
      palette_rev <- rev(brewer.pal(11, "RdYlGn"))
      pal <- colorNumeric(palette = palette_rev,  c(0, colorData))
      radius <- log(weekdata()[[sizeBy]]+2) / log(max(weekdata()[[sizeBy]])+2) * 30000
      leafletProxy("map", data = weekdata()) %>%
        clearShapes() %>% clearMarkers() %>%
        addCircles(~longitude, ~latitude, layerId=~id, radius = radius, #radius=6000,
                   stroke = TRUE, color = "black", weight = 1,
                   fillOpacity = ifelse(weekdata()[[sizeBy]]==0,0,0.8),
                   fillColor=pal(colorData)) %>%
        addMarkers(missing$longitude, missing$latitude, popup = 'No data for selected date range',icon =  myIcon) %>%
        addMarkers(zeros$longitude, zeros$latitude, popup = 'No moths in trap',icon =  myMoth) %>%
        addLegend("topright", pal=pal, values=colorData, title='Count',
                  layerId="colorLegend",opacity = 1)
      
    }
  })
  
  # output$timeMoths <- renderPlot({
  #   p()# p() + geom_vline(xintercept=as.numeric(as.Date(input$date)),colour="black", linetype = "longdash", alpha = 0.5)
  # })
  # 
  # Show a popup at the given location
  showZipcodePopup <- function(id, lat, lng) {
    selectedZip <- weekdata()[weekdata()$id == id,]
    
    content <- as.character(tagList(
      # tags$h4("Count:", as.integer(selectedZip$count)),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedZip$location, selectedZip$state
      ))), tags$br(),
      sprintf("Trap operator: %s", selectedZip$operator),tags$br(),
      sprintf("Count: %d", as.integer(selectedZip$count)),tags$br()
    ))
    if(length(selectedZip$count)==0){
      content <-  as.character(tagList(
        tags$h4("No trap data for selected date range")))
    }
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    input$map_shape_click$id
    leafletProxy("map") %>% clearPopups()
    
    if (is.null(input$map_shape_click$id))
      return()
    
    isolate({
      showZipcodePopup(input$map_shape_click$id, input$map_shape_click$lat, input$map_shape_click$lng)
    })
    
  })
  
  observe({
    input$date
    leafletProxy("map") %>% clearPopups()
  })
  
  
  
})
