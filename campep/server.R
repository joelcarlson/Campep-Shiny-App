library(leaflet); library(ShinyDash); library(rCharts); library(maps); library(shiny)
library(sjPlot); library(ggplot2); library(reshape); library(plyr); library(rCharts)

SchoolNames <- getSchoolNames()

shinyServer(function(input, output, session) {

# This will set the initial value of input$schoolname to getSchoolNames()[1,1]
observe({updateTextInput(session, "schoolname", 
                    #label = "Columbia University", 
                    value = getSchoolNames()[1,1])})

  
output$map_container <- renderChart2({
    plotMap()
  })

output$degtype <- renderUI({
    uniquedegs <- as.character(unique(SchoolNames[SchoolNames$School.Name == input$schoolname,2]))
    selectInput('degreetype', 'Degree Type', uniquedegs)
    })
  
output$plot <- renderChart2({
    if(input$plot.selector == "Enrolment"){  
        plot.enrol(input$schoolname, input$degreetype)
    }
    else{
        plot.jobs(input$schoolname, input$degreetype)
    }})

    

    
 output$table <- renderDataTable({
        #Display values from plot tab
        getStats()[,c(-2,-3,-16,-17,-18,-19,-20,-21)]
        
    })  

output$statsTable <- renderTable({
        percents(input$schoolname, input$degreetype)
        })
  })