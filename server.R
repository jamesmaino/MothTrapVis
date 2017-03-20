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
                    as.numeric(format(as.Date(yearweek), '%Y')) == input$myYear))  
    }else{
      return(subset(cleantable1,
                    as.numeric(format(as.Date(yearweek), '%Y')) == input$myYear))
    }
  })
  
  ## Interactive Map ###########################################
  output$yearSlider <- renderUI({
    subct <- subset(zipdata(), format(zipdata()$yearweek, '%Y') == input$myYear)
    if(is.null(input$date)){
      myDates <- c(as.Date(paste0(input$myYear,'-08-20')),as.Date(paste0(input$myYear,'-08-27')))
      }else{
        myDates<-input$date
    }
    sliderInput('date', 'Map showing trap data for date range:', min=min(as.Date(subct$yearweek)), max=max(as.Date(subct$yearweek)), value = myDates, width = '100%', step = 1)
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
        addMarkers(missing$longitude, missing$latitude, popup = 'No data at selected week',icon = myIcon)
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
        addMarkers(missing$longitude, missing$latitude, popup = 'No data for selected date range',icon =  myIcon) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title='Count',
          layerId="colorLegend",opacity = 1)
      
    }
  })

  output$timeMoths <- renderPlot({
    # blank plot
    p<-ggplot(data=data.frame())
    mytheme <- theme_bw()+theme(text = element_text(size=rel(4)),
                     axis.text.x = element_text(size=rel(5),angle = 45, hjust = 1),
                     plot.title = element_text(size = 12, colour = "red"))
    # build trap data
    if(is.null(input$map_shape_click$id)){
      noMapClick = TRUE 
    }else{
      noMapClick = !input$map_shape_click$id %in% unique(zipdata()$id)
    }
    if(noMapClick){
      p = p + ggtitle('Click trap to overlay data')
      trap_dat = NULL
    }else{
      trap_dat = subset(zipdata(), id == input$map_shape_click$id)
      trap_dat$yearweek<-format(trap_dat$yearweek, format = '%Y-w%U')
      if(nrow(trap_dat)>0){
        trap_dat$yearweek<-as.Date(paste(trap_dat$yearweek,7),"%Y-w%U %u") # need to add day
        trap_dat<-trap_dat%>%group_by_('yearweek')%>%summarise(count=sum(count, na.rm = TRUE))
        p = p + geom_bar(data=trap_dat, aes(x=yearweek, y=count, width=7), alpha = 0.5, stat="identity",fill = '#F8766D', show.legend = TRUE, position=position_dodge())
      }
    }
    
    # set up state wide summary
    if(input$region == 'all states'){
      sdf<-zipdata()
    }else{
      sdf<-subset(zipdata(),state == input$region)
    }
    sdf$yearweek<-format(sdf$yearweek, format = '%Y-w%U')
    if(nrow(sdf)>0)sdf$yearweek<-as.Date(paste(sdf$yearweek,7),"%Y-w%U %u") # need to add day
    trapNo = length(unique(sdf$id))
    state_dat<-sdf%>%group_by_('yearweek')%>%
      summarise(se=sd(count)/sqrt(sum(!is.na(count))),count=mean(count, na.rm = TRUE))
    state_dat$lwr <- state_dat$count - state_dat$se
    state_dat$upr <- state_dat$count + state_dat$se
    if(nrow(state_dat)>0){
      p = p +
        geom_line(data=state_dat, aes(x=yearweek, y=count), show.legend = TRUE) +
        geom_ribbon(data=state_dat,aes(x = yearweek, y=count, ymin=lwr,ymax=upr),alpha = 0.3) +
        xlab('') +
        ylab(paste0('Mean weekly count of \n',trapNo,' traps in ',input$region, " (",format(mean(sdf$yearweek), format = '%Y'),")")) 
      #+   scale_x_date(limits = c(min(sdf$yearweek)-7, max(sdf$yearweek)+7), date_breaks = "2 week",date_labels = "%b %d")
    }else{p = p + ggtitle('No data for selected year or region')}
    
    return(p+mytheme)
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
        selectedZip$district, selectedZip$state
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
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    showZipcodePopup(event$id, event$lat, event$lng)
    isolate({
      
      })
  })
  

})
