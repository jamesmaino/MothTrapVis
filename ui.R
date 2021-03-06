library(shiny)
library(leaflet)

# Choices for drop-downs
vars <- c(
  "Hide trend plot" = "hidePlot",
  "Victoria" = "VIC",
  "South Australia" = "SA",
  "New South Wales" = "NSW",
  "Queensland" = "QLD",
  "Western Australia" = "WA",
  "All states" = "all states"
)

vars1 <- c(
  "H. armigera" = "armigera",
  "H. punctigera" = "punctigera",
  "S. frugiperda" = "frugiperda"
)

shinyUI(navbarPage(div(img(src="cesar_logo.png", width = 30, height = 30), "MothTrapVis"), id="nav", windowTitle = "MothTrapVis",
#### interaction panel ####
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
        # includeScript("gomap.js")
      ),
      leafletOutput("map", width="100%", height="100%"),
      fixedPanel(id = 'selections',
                 top = '10%', draggable = FALSE, left = '5%', right = '5%',
                 bottom = "auto", width = 'auto', height = "auto",
                 fluidRow(
                   column(1, selectInput( 'myYear','Year', selected = '2020', choices = 2017:2020)),
                   column(2, selectInput("species", "Species", selected = 'frugiperda', vars1)),
                   column(2, selectInput("region", "Region for graph", selected = 'hidePlot', c(vars))),
                   column(4),
                   column(2,
                          absolutePanel(draggable = TRUE, top = '30%', left = "auto", 
                                        right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        conditionalPanel("input.region != 'hidePlot'",
                                             id = "controls", class = "panel panel-default",
                                             plotOutput("timeMoths", height = 300),
                                             helpText("red bar graph shows data at selected trap\n
                      grey bars show standard error"),
                                             helpText(   a("Click Here for raw data", 
                                                           href="https://docs.google.com/spreadsheets/d/16tksOn7SAv4ezJCX_z4no4MpJDtua6pGqVPjfg57OZ8/edit#gid=0", target="_blank"  )
                                             )
                                             
                            )
                          )
                   )#,
                                # img(src="logo.png", height = 70),
                                # img(src="SARDI_small.png", height = 70),
                                # img(src="DAFWA_small.png",height = 70),
                                # img(src="GRDC_small.png",  height = 70),
                                # img(src="QDAF_small.png", height = 70)
                 )
      ),
      fixedPanel(id = 'sliderPanel',class = "panel panel-default",
                 bottom = '0%', draggable = FALSE, left = '5%', right = '5%',
                 top = "auto", width = 'auto', height = "auto",
                 column(uiOutput('yearSlider'),width = 10, offset = 1)),
      
      
      tags$div(id="cite",'cesar'
      )
    )
  ),
#### Animation map tab ####
  tabPanel("Animation map",
           div(class="outer",
               includeCSS("styles.css"),
               leafletOutput("map2", width="100%", height="100%"),
               fixedPanel(id = 'selections2',
                          top = '10%', draggable = FALSE, left = '5%', right = '75%',
                          bottom = "auto", width = 'auto', height = "auto",
                          fluidRow(
                            column(12,
                                   selectInput("species2", "Species", selected = 'punctigera', vars1,width = '50%'),
                                   dateInput("dateMin2", "Start date",as.Date('2019-06-01'),width = '50%'),
                                   selectInput("binSize2", "Bin size (weeks)", 1:10, 4,width = '50%'),
                                   selectInput("timeSpan2", "Time span (weeks)",1:52, 24,width = '50%'),
                                   span(textOutput('durationWarning2'),style="color:red",width = '50%'),
                                   selectInput("aniSpeed2", "Animation speed (s)", 1:10, 4,width = '50%'),
                                   checkboxInput("showMissing","Show 'no data' as x symbols", TRUE,width = '50%')
                            )
                          )
               ),
               fixedPanel(id = 'images2',
                          top = '10%', draggable = TRUE, left = '50%', right = '5%',
                          bottom = "auto", width = 'auto', height = "auto",
                          img(src="logo.png", height = 70),
                          img(src="SARDI_small.png", height = 70),
                          img(src="DAFWA_small.png",height = 70),
                          img(src="GRDC_small.png",  height = 70),
                          img(src="QDAF_small.png", height = 70)
               ),
               fixedPanel(id = 'sliderPanel2',class = "panel panel-default",
                          bottom = '0%', draggable = FALSE, left = '5%', right = '5%',
                          top = "auto", width = 'auto', height = "auto",
                          column(uiOutput('yearSlider2'),width = 10, offset = 1),
                          column(actionButton("refresh", "",
                                              icon = icon("refresh"),
                                              style="color: #fff; background-color: rgba(255,255,255,0); border-color: rgba(255,255,255,0)"
                          ), width =1, offset =10, align = 'right')
               ),

               tags$div(id="cite2",'cesar'
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
