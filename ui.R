library(shiny)
library(leaflet)


vars1 <- c(
  "H. armigera" = "armigera",
  "H. punctigera" = "punctigera"
)

shinyUI(fluidPage(div(
  "MothTrapVis",
  img(src="logo.png", height = 30),
  img(src="SARDI_small.png", height = 30),
  img(src="DAFWA_small.png",height = 30),
  img(src="GRDC_small.png",  height = 30),
  img(src="QDAF_small.png", height = 30)
), id="nav", windowTitle = "MothTrapVis",
#### interaction panel ####
div(class="outer",
    includeCSS("styles.css"),
    leafletOutput("map", width="100%", height="100%"),
    fixedPanel(id = 'selections',
               top = '5%', draggable = FALSE, left = '5%', right = '5%',
               bottom = "auto", width = 'auto', height = "auto",
               fluidRow(
                 column(4),
                 column(4, selectInput("species", "", selected = WA_set$Default_species, vars1))
                 # column(4)
                 # column(4,
                 #              img(src="logo.png", height = 70),
                 #              img(src="SARDI_small.png", height = 70),
                 #              img(src="DAFWA_small.png",height = 70),
                 #              img(src="GRDC_small.png",  height = 70),
                 #              img(src="QDAF_small.png", height = 70)
                 # )
               )
    ),
    fixedPanel(id = 'sliderPanel',class = "panel panel-default",
               bottom = '0%', draggable = FALSE, left = '5%', right = '5%',
               top = "auto", width = 'auto', height = "auto",
               column(uiOutput('yearSlider'),width = 10, offset = 1)),
    
    
    tags$div(id="cite",'contact: jmaino@cesaraustralia.com'
    )
)


# conditionalPanel("false", icon("crosshair"))
))
