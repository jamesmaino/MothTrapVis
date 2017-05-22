library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Victoria" = "VIC",
  "South Australia" = "SA",
  "New South Wales" = "NSW",
  "Queensland" = "QLD",
  "Western Australia" = "WA",
  "All states" = "all states"
)

vars1 <- c(
  "H. armigera" = "armigera",
  "H. punctigera" = "punctigera"
)

shinyUI(navbarPage(div(img(src="cesar_logo.png", width = 30, height = 30), "MothTrapVis"), id="nav", windowTitle = "MothTrapVis",
  tabPanel("Animation map",
    div(class="outer",
      includeCSS("styles.css"),
      leafletOutput("map", width="100%", height="100%"),
      fixedPanel(id = 'selections',
                 top = '10%', draggable = FALSE, left = '5%', right = '5%',
                 bottom = "auto", width = 'auto', height = "auto",
                 fluidRow(
                   column(2, selectInput("species", "Species", selected = 'punctigera', vars1),                           dateInput("dateMin", "Start date",as.Date('2016-01-01')),
                             dateInput("dateMax", "End date",as.Date('2016-12-31')),
                             numericInput("binSize", "bin size (weeks)",40),
                             numericInput("aniSpeed", "Animation speed (s)",10,))
                 )
      ),
      fixedPanel(id = 'images',
                 top = '10%', draggable = TRUE, left = '50%', right = '5%',
                 bottom = "auto", width = 'auto', height = "auto",
                 img(src="logo.png", height = 70),
                 img(src="SARDI_small.png", height = 70),
                 img(src="DAFWA_small.png",height = 70),
                 img(src="GRDC_small.png",  height = 70),
                 img(src="QDAF_small.png", height = 70)
      ),
      fixedPanel(id = 'sliderPanel',class = "panel panel-default",
                 bottom = '0%', draggable = FALSE, left = '5%', right = '5%',
                 top = "auto", width = 'auto', height = "auto",
                 column(uiOutput('yearSlider'),width = 10, offset = 1),
                 column(actionButton("refresh", "", 
                                     icon = icon("refresh"),
                                     style="color: #fff; background-color: rgba(255,255,255,0); border-color: rgba(255,255,255,0)"
                 ), width =1, offset =10, align = 'right')
                 ),
      
      tags$div(id="cite",'cesar'
      )
    )
  ),


  conditionalPanel("false", icon("crosshair"))
))
