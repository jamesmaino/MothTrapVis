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
  tabPanel("Interactive Map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      leafletOutput("map", width="100%", height="100%"),
      tags$style(type='text/css', ".info legend leaflet-control {  }"), 
      tags$style(type='text/css', ".irs-grid-text { font-size: 10pt; 
                                                    color:rgba(255,255,255,0);}"), 
      tags$style(type='text/css', ".irs-min { font-size: 14pt; }"),
      tags$style(type='text/css', ".irs-max { font-size: 14pt; }"),
      tags$style(type='text/css', ".irs-single { font-size: 14pt; }"), 
      tags$style(type='text/css', ".irs-from { font-size: 14pt; }"), 
      tags$style(type='text/css', ".irs-to { font-size: 14pt; }"), 
      tags$style(type='text/css', "label.control-label { font-size: 14pt; }"),
      fixedPanel(id = 'selections',
                 top = '10%', draggable = FALSE, left = '5%', right = '5%',
                 bottom = "auto", width = 'auto', height = "auto",
                 fluidRow(
                   column(1, selectInput( 'myYear','Year', selected = '2016', choices = unique(format(as.Date(cleantable$yearweek),'%Y')))),
                   column(2, selectInput("species", "Species", selected = 'punctigera', vars1)),
                   column(2, selectInput("region", "Region for graph", selected = 'all states', vars)),
                   # fixedPanel(id = 'logo',class = "panel panel-default",
                   #            top = 'auto', draggable = FALSE, left = 12, bottom = "50%",
                   #            width = 'auto', height = "auto",

                              # fluidRow(
                                img(src="logo.png", height = 70),
                                img(src="SARDI_small.png", height = 70),
                                img(src="DAFWA_small.png",height = 70),
                                img(src="GRDC_small.png",  height = 70),
                                img(src="QDAF_small.png", height = 70)
                                # )
                   #            )
                   
                 )
      ),
      fixedPanel(id = 'sliderPanel',class = "panel panel-default",
                 bottom = '0%', draggable = FALSE, left = '5%', right = '5%',
                 top = "auto", width = 'auto', height = "auto",
                 column(uiOutput('yearSlider'),width = 10, offset = 1)),
      # LOGOS
      # fixedPanel(id = 'logo',class = "panel panel-default",
      #            top = 'auto', draggable = FALSE, left = 12, bottom = "50%",
      #            width = 'auto', height = "auto",
      #            fluidRow(
      #              img(src="logo.png", width = 50, height = 50),
      #              img(src="SARDI_small.png", height = 50),
      #              img(src="DAFWA_small.png",height = 50),
      #              img(src="GRDC_small.png",  height = 50),
      #              img(src="QDAF_small.png", height = 50)
      #              )
      #            ),
      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = '30%', left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        br(),
        tags$head(tags$style(
          HTML('
               #sliderPanel {background-color: rgba(255,255,255,0);
                             outline: none;
                             border: 0;
               #} 
               #yearSlider {outline: none}')
          )),
        plotOutput("timeMoths", height = 300),
        helpText("red bar graph shows data at selected trap\n
                  grey bars show standard error"),
        helpText(   a("Click Here for raw data", 
                      href="https://docs.google.com/spreadsheets/d/16tksOn7SAv4ezJCX_z4no4MpJDtua6pGqVPjfg57OZ8/edit#gid=0", target="_blank"  )
          )
      ),
      
      tags$div(id="cite",'cesar'
      )
    )
  ),
  # tabPanel("About"),
  
  # tabPanel("Regional Summary",
  #          fluidRow(br(),br(),
  #            column(6,
  #                   plotOutput("plotMothNumber")
  #            ), h5("Trap locations used in plot shown below"),
  #            column(5,
  #                   leafletOutput("map1")
  #            )
  #          ),
  #          hr()
  # ),

  conditionalPanel("false", icon("crosshair"))
))
