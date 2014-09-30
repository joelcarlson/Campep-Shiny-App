library(leaflet); library(ShinyDash); library(rCharts); 
library(maps); library(shiny); library(plyr)
SchoolNames <- getSchoolNames()
shinyUI(fluidPage(
#theme = "bootstrap.css",
  fluidRow(column(12,
  chartOutput('map_container', 'leaflet'))),
 
  fluidRow(
    column(8,
      h2('Campep Accreditation Statistics')
     )
  ),
  
  hr(),
  
  fluidRow(
    column(4,
      selectInput('schoolname', 'School Name', levels(getSchoolNames()[,1])),
      uiOutput("degtype"),
      selectInput('plot.selector', 'Plot Select', choices=c("Enrolment", "Graduates"))
      
    ),
    column(8,
        
      hr(),      
      tableOutput('statsTable')
    )  
  ),
    
  hr(),
    
  fluidRow( 
    column(1,
      showOutput("plot", "Highcharts")
      )
  ),
  hr(),
  
  fluidRow(
    column(12, hr(),
        h2("Full Data Table"),
        hr(),
        dataTableOutput("table")
    )
  ) 
        
        ))  



