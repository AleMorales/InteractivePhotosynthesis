library(shinyBS)
library(shiny)


shinyUI(
  fluidPage(
    # CSS style included in the header of the HTML. Used to make multicolumn checkboxgroupinput
    list(tags$head(tags$style(HTML(".multicol { 
                                      height: 150px;
                                      -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                      -moz-column-count: 3;    /* Firefox */ 
                                      column-count: 3;}")))),
    img(src= "Biosolar.png", style="float:right; width:20%"),
    img(src= "WUR.png", style="float:right; width:20%"),
    title = "Interactive Steady-State Photosynthesis",
    h1("Interactive Steady-State Photosynthesis", align = "center"),
    h2("Manage Response Curves"),
    # Panel to add, remove, reset scenarios
    wellPanel(
      # Buttons to do basic operations
      fluidRow(
        actionButton(inputId = "addScenario", label = "Add"),
        actionButton(inputId = "removeScenario", label = "Remove"),
        actionButton(inputId = "renameScenario", label = "Rename"),
        actionButton(inputId = "resetScenario", label = "Reset"),
        actionButton(inputId = "removeAllScenarios", label = "Remove all"),
        actionButton(inputId = "uploadScenario", label = "Open"),
        div(style="display:inline-block", selectInput(inputId = "listManagedScenarios", 
                    label = NULL, choices = "A-PAR C3", selected = "A-PAR C3", selectize = F))
      ),
      # Tooltip associated to the different actionButtons in Scenario management
      bsTooltip(id = "addScenario", title = HTML("Trigger scenario creation panel.")),
      bsTooltip(id = "removeScenario", title = HTML("Remove permanently the scenario currently selected, including the results of the simulation.")),
      bsTooltip(id = "uploadScenario", title = HTML("Add a scenario that was previously downloaded from the app as JSON format, including all input values as well as simulation outputs (if present).")),
      bsTooltip(id = "downloadScenario", title = HTML("Download current scenario (input values and simulation outputs, if present) in a JSON format that can later be uploaded.")),
      bsTooltip(id = "renameScenario", title = HTML("Change the name of the currently selected scenario. If present, simulated outputs will also be updated.")),
      bsTooltip(id = "resetScenario", title = HTML("Resent all inputs of the currently selected scenario to their default. Simulated output will not be updated unless a new simulation is generated.")),
      bsTooltip(id = "removeAllScenarios", title = HTML("Delete all scenarios.")),
      bsTooltip(id = "listManagedScenarios", title = HTML("Choose which scenario you want to work on."),placement = "top"),
      # This only appears when we need to upload a previously downloaded scenario
      fluidRow(column(w = 12, uiOutput(outputId = "fileInputScenario"))),
      # This only appears when we need to name a new scenario
      fluidRow(column(w = 12, uiOutput(outputId = "nameScenario"))),
      # This only appears when we need to rename an existing scenario
      fluidRow(column(w = 12, uiOutput(outputId = "renameScenario"))),
      # These are anchors to show error messages on the app whenever something goes wrong
      fluidRow(column(w = 12, bsAlert(anchorId = "errorNameScenario"))),
      fluidRow(column(w = 12, bsAlert(anchorId = "errorRenameScenario")))
    ),
    
    # Panel to select environmental variables
    fluidRow(
      column(4,
             wellPanel( 
               fluidRow(column(w = 12, selectInput(inputId = "chooseEnvironment",label = "Environmental variables",
                                                   choices = c("PAR", "Tleaf", "VPD", "Ca", "O2"), selected = "PAR"))),
               fluidRow(column(w = 12, htmlOutput(outputId = "environmentDescription"))),
               fluidRow(column(w = 12,textInput(inputId = "valueEnv", label = NULL, value = paste(curveVariables[["PAR"]], collapse = " "))),
                        bsTooltip(id = "valueEnv", title = "Input the values separated by whitespace.")),
               fluidRow(column(w = 12, 
                               actionButton(inputId = "updateEnv",label = "Update"),
                               actionButton(inputId = "resetEnv",label = "Reset"),
                               bsTooltip(id = "updateEnv", title = "Assign the values to the selected environmental variable."),
                               bsTooltip(id = "resetEnv", title = "Reset environmental variable to default value."))
               ),
               fluidRow(bsAlert(anchorId = "errorStringEnvironment")),
               fluidRow(bsAlert(anchorId = "errorMultipleEnvironment"))
             )
      ),
      
      
      # Panel to select parameters
      column(4, 
             wellPanel(
               fluidRow(column(w = 12, selectInput(inputId = "chooseParameters",label = "Parameters",
                                                   choices = namesC3Parameters, selected = "Vcmax25"))),
               fluidRow(column(w = 12, htmlOutput(outputId = "parameterDescription"))),
               fluidRow(column(w = 12,textInput(inputId = "valueParameters",label = "",value = NULL)),
                        bsTooltip(id = "valueParameters", title = "Input value for selected parameter.")), 
               fluidRow(column(w = 4, actionButton(inputId = "updateParameters",label = "Update")),
                        column(w = 4, actionButton(inputId = "resetParameters",label = "Reset")),
                        bsTooltip(id = "updateParameters", title = "Assign the value to the selected parameter."),
                        bsTooltip(id = "resetParameters", title = "Reset parameter to its default value.")),
               fluidRow(bsAlert(anchorId = "errorStringParameter")),
               fluidRow(bsAlert(anchorId = "errorMultipleParameter")),
               fluidRow(bsAlert(anchorId = "errorRangeParameter"))
             )
      ),
      
      # Run simulation for scenario
      column(w = 4, actionButton(inputId = "runScenario", label = "Calculate"),
                    downloadButton(outputId = "downloadScenario", label = "Save as...")),
      bsTooltip(id = "runScenario", title = "Run simulation for the scenario currently selected."),
      column(w = 2, bsAlert(anchorId = "errorRunScenario"))
    ),
    
    
    # Visualize scenarios ------------------------------------------------------------      
    
    h2("Visualize Curves"),
    
    # Row to select the scenarios to be plotted and number of plots
    # Add some css styling to make sure we have multicolumn arrangement
    fluidRow(
      column(w = 10, uiOutput(outputId = "analysedScenarios")),
      column(w = 2, numericInput(inputId = "nplots",label = "No. figures",min = 0, max = 6,value = 1))
    ),
    
    # Plots
    fluidRow(
      conditionalPanel("input.nplots > 0", column(w = 6, interactivePlotInput(id = "plot1",variables = allVariables, defaultX = "PAR",defaultY = "An"))),
      conditionalPanel("input.nplots > 1", column(w = 6, interactivePlotInput(id = "plot2",variables = allVariables, defaultX = "PAR",defaultY = "gs"))),
      conditionalPanel("input.nplots > 2", column(w = 6, interactivePlotInput(id = "plot3",variables = allVariables, defaultX = "PAR",defaultY = "Tr"))),
      conditionalPanel("input.nplots > 3", column(w = 6, interactivePlotInput(id = "plot4",variables = allVariables, defaultX = "PAR",defaultY = "WUE"))),
      conditionalPanel("input.nplots > 4", column(w = 6, interactivePlotInput(id = "plot5",variables = allVariables, defaultX = "PAR",defaultY = "Ci"))),
      conditionalPanel("input.nplots > 5", column(w = 6, interactivePlotInput(id = "plot6",variables = allVariables, defaultX = "PAR",defaultY = "Cc"))))
  )
)