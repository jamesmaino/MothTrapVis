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
                    as.numeric(format(as.Date(yearweek), '%Y')) == input$myYear))  
    }else{
      return(subset(cleantable1,
                    as.numeric(format(as.Date(yearweek), '%Y')) == input$myYear))
    }
  })
  animationOptions(interval = 1000, loop = FALSE, playButton = 'p')
  output$yearSlider <- renderUI({
    subct <- subset(zipdata(), format(zipdata()$yearweek, '%Y') == input$myYear)
    # if(is.null(input$date)){
    #   
    #   myDates <- c(as.Date(paste0(input$myYear,'-08-20')),as.Date(paste0(input$myYear,'-08-27')))
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
      setView(lng = 142, lat = -35, zoom = 6)
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
    iconUrl = "x-512.png",
    iconWidth = 20, iconHeight = 20)
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
      legMax<-legendMax()
      palette_rev <- rev(brewer.pal(11, "RdYlGn"))
      pal <- colorNumeric(palette = palette_rev,  c(0, colorData, legMax))
      radius <- log(weekdata()[[sizeBy]]+2) / log(max(weekdata()[[sizeBy]])+2) * 30000
      leafletProxy("map", data = weekdata()) %>%
        clearShapes() %>% clearMarkers() %>%
        addCircles(~longitude, ~latitude, layerId=~id, radius = radius, #radius=6000,
                   stroke = TRUE, color = "black", weight = 1,
                   fillOpacity = ifelse(weekdata()[[sizeBy]]==0,0,0.8),
                   fillColor=pal(colorData)) %>%
        addMarkers(missing$longitude, missing$latitude, popup = 'No data for selected date range',icon =  myIcon) %>%
        addMarkers(zeros$longitude, zeros$latitude, popup = 'No moths in trap',icon =  myMoth) %>%
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
        p = p + geom_bar(data=trap_dat, aes(x=yearweek, y=count), alpha = 0.5, stat="identity",fill = '#F8766D', show.legend = TRUE, position=position_dodge())
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
        geom_ribbon(data=state_dat,aes(x = yearweek, ymin=lwr,ymax=upr),alpha = 0.3) +
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
  
  

  
  ## Animation 
  
  ## Animation Map ###########################################
  zipdata2<- reactive({
    if(input$species=='punctigera'){
      return(cleantable)  
    }else{
      return(cleantable1)
    }
  })
  
  output$yearSlider2 <- renderUI({
    input$refresh
    sliderInput('date2', 'Map showing trap data for date range:', min=input$dateMin2, max=input$dateMin2+as.numeric(input$timeSpan2)*7, value = c(input$dateMin2, input$dateMin2+as.numeric(input$binSize2)*7), width = '100%', step = as.numeric(input$binSize2)*7,animate = animationOptions(interval = as.numeric(input$aniSpeed2)*1000/(as.numeric(input$timeSpan2)/as.numeric(input$binSize2)),loop=TRUE))

  })
  
  output$durationWarning2<-renderText({
    if(as.numeric(input$timeSpan2)%%as.numeric(input$binSize2)!=0){
      return('warning: time span not divisible by bin size')
    }else{
      return(NULL)
    }
  })
  # Create the map
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=b44e15e05f4d4aab955704c3e6f57f2f",
        attribution = 'Maps <a href="http://www.thunderforest.com/">© Thunderforest</a>, Data <a href="http://www.openstreetmap.org/copyright">© OpenStreetMap contributors</a> '
      ) %>%
      setView(lng = 142, lat = -35, zoom = 6)
  })
  # set max of the legend for animation (so doesn't jump around)
  legendMax<-reactive({
    # browser()
    df <- subset(zipdata2(), date >=input$dateMin2&
                 date<=as.Date(input$dateMin2+as.numeric(input$timeSpan2)*7, origin = '1970-01-01' )&
                 latitude<input$map2_bounds$north&
                 latitude>input$map2_bounds$south&
                 longitude<input$map2_bounds$east&
                 longitude>input$map2_bounds$west
                 )
    if(nrow(df)>1){
      df$variablebin<-NA
      datebins<-seq(input$dateMin2,as.Date(input$dateMin2+as.numeric(input$timeSpan2)*7, origin = '1970-01-01'), by = 7*ifelse(is.na(as.numeric(input$binSize2)),1,as.numeric(input$binSize2)))
      count = 1
      for(datebin in rev(datebins)){
        df$variablebin[as.numeric(df$date)<datebin]<-count
        count = count+1
      }
      count<-df%>%group_by_('id','variablebin')%>%summarise(count=sum(count, na.rm = TRUE))
      return(max(count$count))
    }else{
      return(0)  
    }
  })
  # set up trap data for binned time
  weekdata2<- reactive({
    start_date <- input$date2[1]
    end_date <- input$date2[2]
    swd<-subset(zipdata2(), 
                date >=start_date&
                date<=end_date&
                latitude<input$map2_bounds$north&
                latitude>input$map2_bounds$south&
                longitude<input$map2_bounds$east&
                longitude>input$map2_bounds$west  
                )
    swd%>%group_by_('id','latitude','longitude',"operator", "state", "location")%>%summarise(count=sum(count, na.rm = TRUE))
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.

  colorBy <- 'count'
  sizeBy <-  'count'
  observe({
    if (nrow(weekdata2())==0){
      rowsToFind <- weekdata2()[,c('longitude','latitude')]
      yearLonLat<-zipdata2()[,c('yearweek','longitude','latitude')]
      yearLonLat$year<-format(yearLonLat$yearweek, '%Y')
      missing<-negate_match_df(unique(yearLonLat[,c('year','longitude','latitude')]), rowsToFind)
      missing<-missing[missing$year==format(input$dateMin2,'%Y'),]
      leafletProxy("map2", data = weekdata2()) %>%
        clearShapes() %>% clearMarkers()  %>%
        clearControls()
      
      if(input$showMissing){
        rowsToFind <- weekdata2()[,c('longitude','latitude')]
        yearLonLat<-zipdata2()[,c('yearweek','longitude','latitude')]
        yearLonLat$year<-format(yearLonLat$yearweek, '%Y')
        missing<-negate_match_df(unique(yearLonLat[,c('year','longitude','latitude')]), rowsToFind)
        missing<-missing[missing$year==format(input$dateMin2,'%Y'),]
        addMarkers(leafletProxy("map2", data = weekdata2()) ,missing$longitude, missing$latitude,icon =  myIcon) 
      }

    }else{
      colorData <- c(weekdata2()[[colorBy]])
      zeros=subset(weekdata2(), count ==0)
      legMax<-legendMax()
      palette_rev <- rev(brewer.pal(11, "RdYlGn"))
      pal <- colorNumeric(palette = palette_rev, c(0, colorData, legMax))
      radius <- 6^3/input$map2_zoom^3*log(weekdata2()[[sizeBy]]+2) / log(max(legMax)+2) * 30000
      # browser()
      leafletProxy("map2", data = weekdata2()) %>%
        clearShapes() %>% clearMarkers() %>%
        addCircles(~longitude, ~latitude, layerId=~id, radius = radius, #radius=6000,
                   stroke = TRUE, color = "black", weight = 1,
                   fillOpacity = ifelse(weekdata2()[[sizeBy]]==0,0,1),
                   fillColor=pal(colorData)) %>%
        addMarkers(zeros$longitude, zeros$latitude,icon =  myMoth) %>%
        # need to add extra values to legend or it 
        addLegend("topright", pal=pal, values=c(0, colorData, legMax), title=paste('Count <br>', 
                                                                                   format(as.Date(input$date2[1]), format = '%d %b'),'-<br>',
                                                                                   format(as.Date(input$date2[2]),format = '%d %b')),
                  layerId="colorLegend",opacity = 1)
      if(input$showMissing){
        rowsToFind <- weekdata2()[,c('longitude','latitude')]
        yearLonLat<-zipdata2()[,c('yearweek','longitude','latitude')]
        yearLonLat$year<-format(yearLonLat$yearweek, '%Y')
        missing<-negate_match_df(unique(yearLonLat[,c('year','longitude','latitude')]), rowsToFind)
        missing<-missing[missing$year==format(input$dateMin2,'%Y'),]
        addMarkers(leafletProxy("map2", data = weekdata2()) ,missing$longitude, missing$latitude,icon =  myIcon)
      }
      # if(input$date2[1]>'2016-09-20')browser()

    }
  })

  
})
