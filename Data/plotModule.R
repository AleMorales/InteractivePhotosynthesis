library(ggplot2)

# Shiny module encapsulating the functionality associated

# UI
interactivePlotInput = function(id, variables, defaultX, defaultY) {
  # Create a namespace function using the provided id
  ns = NS(id)
  
  tagList(
    wellPanel(
      fluidRow(column(w = 6,  selectInput(inputId = ns("selectX"),label = "X axis",choices = variables, selected = defaultX)),
               column(w = 6,  selectInput(inputId = ns("selectY"),label = "Y axis",choices = variables, selected = defaultY))),
      fluidRow(column(w = 12, htmlOutput(outputId = ns("labelsDescriptions")))),
      fluidRow(column(w = 12, plotOutput(outputId = ns("plot"))))
    )
  )
}

# Server
# Outputs should already be filtered according to listOutputs
interactivePlot = function(input, output, session, Outputs, Description) {
   
  # Add tooltip with a description of a parameter and its units. The behaviour is inconsistent
  output$labelsDescriptions = renderUI({
    HTML(paste0(input$selectX,": ",allLabels[[input$selectX]], " <br> ",
                input$selectY,": ",allLabels[[input$selectY]]))
  })
  
  observe({
    curveType = unique(Outputs$CurveType)[1]
    updateSelectInput(session = session, inputId = "selectX", selected = curveType)
  })
  
  output$plot = renderPlot({
    
    # When nothing is selected, plot nothing
    if(nrow(Outputs) == 0) return(NULL)
    
    # Because renderPlot will run before the observer...
    if(input$selectX == "") return(NULL)
    
    # Actual data to be plotted
    Outputs = Outputs[,c("Scenario", input$selectX, input$selectY)]
    
    # Number of groups
    scenarios = unique(Outputs[,"Scenario"])
    n = length(scenarios)

    # Ranges
    ylim = range(Outputs[,input$selectY])*c(0.95,1.05)
    xlim = range(Outputs[,input$selectX])*c(0.95,1.05)
    
    # First series
    par(xaxs = "i", yaxs = "i", mar = c(4.5,4.5,2*((n + 2) %/% 3),0.5), xpd = T, las = 1)
    data = subset(Outputs, Scenario == scenarios[1])
    plot(data[,input$selectX], data[,input$selectY], t = "l", lwd = 2, pch = 16,
        xlim = xlim, ylim = ylim, 
        xlab = input$selectX, ylab = input$selectY)
    
    # Rest of series
    if(n > 1) {
      for(i in 2:n) {
        data = subset(Outputs, Scenario == scenarios[i])
        points(data[,input$selectX], data[,input$selectY], t = "l", col = i, lwd = 2,pch = 16)
      }
    }
      
    # Legend
    legend("top", inset = c(0,-0.1*((n + 2) %/% 3)),
           legend = substr(x = scenarios, start = 1, stop = 10), col = 1:n, lty = 1, pch = 16, lwd = 2, ncol = 3, bty = "n")
    
  })
}

