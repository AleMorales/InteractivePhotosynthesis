library(shiny)
library(stringr)
library(jsonlite)

# Global variables required for managing simulations ----------------------


allScenarios <- new.env()
allScenarios[["A-PAR C3"]] = C3default
allScenarios[["A-PAR C3"]][["PAR"]] = curveVariables[["PAR"]]

allPhotosType <- new.env()
allPhotosType[["A-PAR C3"]] = "C3"

allCurveTypes <- new.env()
allCurveTypes[["A-PAR C3"]] = "PAR"

allOutputs = data.frame(Scenario = character(0), PAR = numeric(0), Ca = numeric(0), VPD = numeric(0),
                        CO2 = numeric(0), O2 = numeric(0), An = numeric(0), Ac = numeric(0),
                        Aj = numeric(0), Ap = numeric(0), Ag = numeric(0), gs = numeric(0),
                        Ci = numeric(0), Cc = numeric(0), Tr = numeric(0), WUE = numeric(0))

# Server function ---------------------------------------------------------


shinyServer(function(input, output, session) {
  
  # List of reactive values that are used to communicated the different parts of the Shiny app
  reactives = reactiveValues(
    flagScenarios = 1, # Dummy Reactive flag used to indicate that the list of managed scenarios has been changed
    allScenarios = allScenarios, # Turn the server global allScenarios variable into a reactive value
    flagOutputs = 1, # Reactive flag used to indicate a change in the list of scenarios that have been simulated
    allOutputs = allOutputs, # Turn the server global allOutputs variables into a reactive value
    currentManagedScenario = "A-PAR C3", # Reactive values indicating the current scenario being managed
    allPhotosType = allPhotosType, # Turn the server global allPhotosType into a reactive value
    allCurveTypes = allCurveTypes,
    nameScenarioFlag = FALSE,  # Dummy reactive flag to indicate whether the UI for naming a scenario should appear or not
    wrongName = FALSE, # Boolean flag that indicates that the user tried to create an scenario but the validation of the name did not pass
    renameScenarioFlag = FALSE,
    flagReset = 1, # Dummy reactive flag used to indicate that the current scenario has been reset
    wrongRename = FALSE,
    errorSimulation = FALSE, # Boolean flag to detect errors when running a scenario
    uploadScenarioFlag = FALSE
  )
  
  # Update listManagedScenarios. Triggered by reactive flagScenarios and allScenarios
  observe({
    reactives$flagScenarios
    updateSelectInput(session = session, inputId = "listManagedScenarios", label = "",
                choices = ls(reactives$allScenarios), selected = isolate(reactives$currentManagedScenario))
  })

  # Update the currentManagedScenario to the value chosen in the listManagedScenarios dropdown box
  # Only make sense once the listManagedScenarios is created and as long as it is not empty
  observe({
    req(input$listManagedScenarios)
    reactives$currentManagedScenario = input$listManagedScenarios
  }) 
  
  # Download inputs and outputs of a scenario as a list serialized as JSON
  output$downloadScenario = downloadHandler(
    
    filename = function() {
      paste0(reactives$currentManagedScenario,".txt")
    },
    
    content = function(file) {
      Name = input$listManagedScenarios
      CurveType = reactives$allCurveTypes[[input$listManagedScenarios]]
      PhotosType = reactives$allPhotosType[[input$listManagedScenarios]]
      Inputs = reactives$allScenarios[[input$listManagedScenarios]]
      fixedInputs = Inputs[-which(names(Inputs) == CurveType)]
      tableFixedInputs = data.frame(Input = names(fixedInputs), Value = unlist(fixedInputs))
      variableInputs = Inputs[which(names(Inputs) == CurveType)]
      tableVariableInputs = cbind(unlist(variableInputs))
      colnames(tableVariableInputs) = names(variableInputs)
      writeScenario(fileName = file, 
                    Name = Name,
                    PhotosType = PhotosType,
                    CurveType = CurveType,
                    tableFixedInputs = tableFixedInputs, 
                    tableVariableInputs = tableVariableInputs,
                    tableOutputs = as.data.frame(subset(reactives$allOutputs, Scenario == input$listManagedScenarios)))
    }
  )
  
  # Trigger uploadScenarioFlag by clicking on th upload button
  observeEvent(input$uploadScenario, {
    if (reactives$uploadScenarioFlag)
      reactives$uploadScenarioFlag = FALSE
    else
      reactives$uploadScenarioFlag = TRUE
  })  
  
  # Create the UI elements required to upload an scenario that had been previously downloaded
  output$fileInputScenario = renderUI({
    if (reactives$uploadScenarioFlag) {
      return(fluidRow(column(12,fileInput(inputId = "browseFile", label = "Browse file"))))
    } else {
      return()
    }
  }) 
  
  # Upload a JSON file containing a scenario previously download from the app
  observe({
    req(input$browseFile)
    # Parse data stored as JSON
    newScenario = readScenario(input$browseFile[,"datapath"])#fromJSON(txt = input$browseFile[,"datapath"])
    # Extract the data and assign to temporary variables
    name = newScenario$Name
    photostype = newScenario$PhotosType
    curvetype = newScenario$CurveType
    inputs = newScenario$Inputs
    results = newScenario$Outputs
    # Populate apps with the new data and triggered all required flags
    reactives$flagScenarios = isolate(reactives$flagScenarios) + 1
    reactives$flagOutputs = isolate(reactives$flagOutputs) + 1
    reactives$allPhotosType[[name]] = photostype
    reactives$allCurveTypes[[name]] = curvetype
    reactives$allScenarios[[name]] = inputs
    if (length(results) > 0)
      reactives$allOutputs = rbind(isolate(reactives$allOutputs), results)
    reactives$uploadScenarioFlag = FALSE
  })
  
  
  # Reaction to the remove button
  observeEvent(input$removeScenario, {
    # Grab the current scenario
    current = input$listManagedScenarios
    if(!is.null(current)) {
      # Remove current scenario from the allScenarios reactive and trigger change in flagScenarios
      if (current %in% ls(reactives$allScenarios)) {
        rm(list = current, envir = reactives$allScenarios)  
        reactives$flagScenarios = reactives$flagScenarios + 1
      }
      # Remove simulated scenario (if present) and trigger changed in flagOutputs
      if (current %in% unique(reactives$allOutputs$Scenario)) {
        reactives$allOutputs = reactives$allOutputs[-which(reactives$allOutputs$Scenario == current),]
        reactives$flagOutputs = reactives$flagOutputs + 1
      }      
    }
  })
  
  
  # Remove all scenarios Trigger the necessary flags to propagate changes to the rest of the app
  observeEvent(input$removeAllScenarios, {
    rm(list = ls(reactives$allScenarios), envir = reactives$allScenarios)
    reactives$flagScenarios = reactives$flagScenarios + 1
    reactives$allOutputs = allOutputs
    reactives$flagOutputs = reactives$flagOutputs + 1
  })
  
  # Add scenario button will trigger a reactive flag required to display the UI element for naming the new scenario
  observeEvent(input$addScenario, {
    if (reactives$nameScenarioFlag)
      reactives$nameScenarioFlag = FALSE
    else
      reactives$nameScenarioFlag = TRUE
  })  
  
  
  # Create relevant UI element when need to name a scenario
  output$nameScenario = renderUI({
    if (reactives$nameScenarioFlag) {
      return(fluidRow(column(w = 4,textInput(inputId = "scenarioName",label = "Name new scenario",value = "")),
                      column(w = 1, actionButton(inputId = "validateName",label = "Add")),
                      column(w = 1, radioButtons(inputId = "chooseC3C4", label = "Type",
                                                 choices = c("C3", "C4"), selected = "C3")),
                      column(w = 2, radioButtons(inputId = "chooseCurveType", label = "Curve",
                                                  choices = c("PAR", "Tleaf", "VPD", "Ca", "O2"),
                                                  selected = "PAR")),
                      # Tooltips associated to the nameScenario UI
                      bsTooltip(id = "scenarioName", title = HTML("Assign the name to the new curve It must be different from the names of existing curves")),
                      bsTooltip(id = "validateName", title = HTML("Add a new curve to the list of managed curves")),
                      bsTooltip(id = "chooseC3C4", title = HTML("Choose the type of photosynthesis."), placement = "right"),
                      bsTooltip(id = "chooseCurveType", title = HTML("Choose the type of response curve to be calculated."), placement = "right")))
    } else {
      return()
    }
  })  
  
  
  # Try to create a new scenario. It will check whether the name already exists and if not it will add the scenario.
  # If it fails, set on a boolean flag. If it does not fail, the scenario is created and the naming UI disappears 
  observeEvent(input$validateName, {
    if (!(input$scenarioName %in% ls(reactives$allScenarios)) && (input$scenarioName != "")) {
      reactives$flagScenarios = reactives$flagScenarios + 1
      reactives$allPhotosType[[input$scenarioName]] = input$chooseC3C4
      reactives$allCurveTypes[[input$scenarioName]] = input$chooseCurveType
      if(reactives$allPhotosType[[input$scenarioName]] == "C3") {
        reactives$allScenarios[[input$scenarioName]] = C3default 
      } else {
        reactives$allScenarios[[input$scenarioName]] = C4default
      }
      reactives$allScenarios[[input$scenarioName]][[input$chooseCurveType]] = curveVariables[[input$chooseCurveType]]
      reactives$nameScenarioFlag = FALSE
      reactives$wrongName = FALSE
      reactives$currentManagedScenario = input$scenarioName
    } else {
      reactives$wrongName = TRUE
    }
  }) 
  
  
  # When wrongName = TRUE open alert. When turn off, close it
  observe({
    if (reactives$wrongName == TRUE) {
      createAlert(session,  anchorId = "errorNameScenario", alertId = "alertNameScenario", title = "Error!",
                  content = "Not a valid name (perhaps it already exists?)", style = "danger")
    } else if (reactives$wrongName == FALSE) {
      closeAlert(session,  alertId = "alertNameScenario")
    }
  })
  
  
  # Rename scenario button will create new ui elements to type the new name of scenario.
  # This is achieved indirectly by manipulating a reactive boolean flag
  observeEvent(input$renameScenario, {
    if (reactives$renameScenarioFlag)
      reactives$renameScenarioFlag = FALSE
    else
      reactives$renameScenarioFlag = TRUE
  })  
  
  
  # Create the UI elements required to rename an existing scenario
  output$renameScenario = renderUI({
    if (reactives$renameScenarioFlag) {
      return(fluidRow(column(4,textInput(inputId = "scenarioRename",label = "Rename scenario",value = "")),
                      column(2, actionButton(inputId = "validateRename",label = "Rename")),
                      # Tooltips associated to the nameScenario UI
                      bsTooltip(id = "scenarioRename", title = HTML("Assign new name to the new scenario. It must be different from the names of existing scenarios.")),
                      bsTooltip(id = "validateRename", title = HTML("Rename the scenario."))))
    } else {
      return()
    }
  })    
  
  
  # Try to rename the scenario currently being managed. It will check whether the name already exists and if not it will rename the scenario.
  # If it fails, set on a boolean flag. If it does not fail, the scenario is renamed and the naming UI disappears 
  # It will also rename the output associated to the scenario
  observeEvent(input$validateRename, {
    if (!(input$scenarioRename %in% ls(reactives$allScenarios)) && (input$scenarioRename != "")) {
      # Activate flags to trigger changes that depend on list of scenarios and outputs
      reactives$flagScenarios = reactives$flagScenarios + 1
      # Create a new scenario with the new name and copy the contents of the scenario with the older name
      reactives$allScenarios[[input$scenarioRename]] =  reactives$allScenarios[[input$listManagedScenarios]]
      # Remove the scenario with the older name
      rm(list = input$listManagedScenarios, envir = reactives$allScenarios)
      # Repeat same operation for the photosynthesis type
      reactives$allPhotosType[[input$scenarioRename]] =  reactives$allPhotosType[[input$listManagedScenarios]]
      reactives$allCurveTypes[[input$scenarioRename]] =  reactives$allCurveTypes[[input$listManagedScenarios]]
      rm(list = input$listManagedScenarios, envir = reactives$allPhotosType)
      rm(list = input$listManagedScenarios, envir = reactives$allCurveTypes)
      # Repeat same operation if the scenario is also present in the list of outputs
      if (input$listManagedScenarios %in% unique(reactives$allOutputs$Scenario)) {
        reactives$allOutputs[which(reactives$allOutputs$Scenario == input$listManagedScenarios),"Scenario"] =
          input$scenarioRename
        reactives$flagOutputs = reactives$flagOutputs + 1
      }
      reactives$renameScenarioFlag = FALSE
      reactives$wrongRename = FALSE
    } else {
      reactives$wrongRename = TRUE
    }
  })    
  
  
  # Alert that appears in the app when we failed to rename a scenario
  observe({
    if (reactives$wrongRename == TRUE) {
      createAlert(session,  anchorId = "errorRenameScenario", alertId = "alertRenameScenario", title = "Error!",
                  content = "Not a valid name (perhaps it already exists?)", style = "danger")
    } else if (reactives$wrongRename == FALSE) {
      closeAlert(session,  alertId = "alertRenameScenario")
    } 
  })    
  
  
  # Reset a scenario to its default values
  observeEvent(input$resetScenario,{
    req(reactives$allPhotosType)
    if(reactives$allPhotosType[[reactives$currentManagedScenario]] == "C3") {
      reactives$allScenarios[[reactives$currentManagedScenario]] = C3default
    } else {
      reactives$allScenarios[[reactives$currentManagedScenario]] = C4default
    }
    # Assign the default value to the environmental variables that defines the response curve
    reactives$allScenarios[[reactives$currentManagedScenario]][[reactives$allCurveTypes[[reactives$currentManagedScenario]]]] = 
      curveVariables[[reactives$allCurveTypes[[reactives$currentManagedScenario]]]]
    reactives$flagReset = reactives$flagReset + 1
  })  
  
  ###########################
  # Environmental variables #
  ###########################
  # Update value for environmental variable when a new variable or scenario are chosen or when a scenario was reset
  observe({
    reactives$flagReset
    scenarioInputs = reactives$allScenarios[[reactives$currentManagedScenario]]
    selectedEnv = scenarioInputs[[input$chooseEnvironment]]
    updateTextInput(session = session, inputId = "valueEnv", label = NULL, 
                    value =  paste(selectedEnv, collapse = " "))
    closeAlert(session,  alertId = "alertStringEnvironment")
    closeAlert(session,  alertId = "alertMultipleEnvironment")    
  })
  
  # Update button to assign environmental variables to the scenario
  observeEvent(input$updateEnv, {
    closeAlert(session,  alertId = "alertStringEnvironment")
    closeAlert(session,  alertId = "alertMultipleEnvironment")
    req(input$valueEnv)
    new_value = as.numeric(strsplit(str_trim(input$valueEnv), split = " ")[[1]])
    # Check that a valid number (or series of numbers) are assigned to the environmental factor
    if(any(is.na(new_value))) {
      createAlert(session,  anchorId = "errorStringEnvironment", alertId = "alertStringEnvironment", title = "Error!",
                  content = "You cannot assign a string to an environmental variable, only numbers please.", style = "danger")
      return(NULL)
    }
    # If the selected environmental factor is no the driving force of the response curve, it should only have a single value
    type = reactives$allCurveTypes[[reactives$currentManagedScenario]]
    if(input$chooseEnvironment != type) {
      if(length(new_value) != 1) {
        createAlert(session,  anchorId = "errorMultipleEnvironment", alertId = "alertMultipleEnvironment", title = "Error!",
                  content = paste0("For the current type of response curve only ",type, " can have more than one value."), style = "danger")
        return(NULL)
      }
    }
    
    reactives$allScenarios[[reactives$currentManagedScenario]][[input$chooseEnvironment]] = new_value
  })  
  
  # Reset button will restore the selected environmental variable to the default values
  observeEvent(input$resetEnv, {
    closeAlert(session,  alertId = "alertStringEnvironment")
    closeAlert(session,  alertId = "alertMultipleEnvironment")
    if(input$chooseEnvironment == reactives$allCurveTypes[[reactives$currentManagedScenario]]) {
      selectedEnv = curveVariables[[input$chooseEnvironment]]
    } else {
      selectedEnv = C3default[[input$chooseEnvironment]]
    }
    updateTextInput(session = session, inputId = "valueEnv", label = NULL, 
                    value =  paste(selectedEnv, collapse = " "))
    reactives$allScenarios[[reactives$currentManagedScenario]][[input$chooseEnvironment]] = selectedEnv
  })
  
  # Add tooltip with a description of a variable and its units. The behaviour is inconsistent
  output$environmentDescription = renderUI({
    HTML(descriptionEnvironment[[input$chooseEnvironment]])
  })
  
  
  ##############
  # Parameters #
  ##############
  # Update parameter value when a new parameter is chosen
  observe({
    scenarioInputs = isolate(reactives$allScenarios[[reactives$currentManagedScenario]])
    selectedPar = scenarioInputs[[input$chooseParameters]]
    updateTextInput(session = session, inputId = "valueParameters", label = NULL, 
                    value =  paste(selectedPar, collapse = " "))
    closeAlert(session,  alertId = "alertStringParameter")
    closeAlert(session,  alertId = "alertMultipleParameter")
    closeAlert(session,  alertId = "alertRangeParameter")
  }) 
  
  # Update parameters when a new scenario is used, resetted or updated
  observe({
    reactives$flagReset
    scenarioInputs = reactives$allScenarios[[reactives$currentManagedScenario]]
    if (reactives$allPhotosType[[reactives$currentManagedScenario]] == "C3") {
      updateSelectInput(session = session, inputId = "chooseParameters",label = "Parameters",
                        choices = namesC3Parameters, selected = "Vcmax25")
    } else {
      updateSelectInput(session = session, inputId = "chooseParameters",label = "Parameters",
                        choices = namesC4Parameters, selected = "Vcmax25")
    }
    selectedPar = scenarioInputs[["Vcmax25"]]
    updateTextInput(session = session, inputId = "valueParameters", label = NULL, 
                    value =  paste(selectedPar, collapse = " "))
  })       
  
  # Update button to assign parameter to the scenario
    observeEvent(input$updateParameters, {
    closeAlert(session,  alertId = "alertStringParameter")
    closeAlert(session,  alertId = "alertMultipleParameter")
    closeAlert(session,  alertId = "alertRangeParameter")
    
    req(input$valueParameters)
    new_value = as.numeric(strsplit(str_trim(input$valueParameters), split = " ")[[1]])
    
    # Check that a proper number was assigned to the parameter
    if (any(is.na(new_value))) {
      createAlert(session,  anchorId = "errorStringParameter", alertId = "alertStringParameter", title = "Error!",
                  content = "You cannot assign a string to a parameter, only numbers please.", style = "danger")
      return(NULL)
    }
    
    # Check that only one value was assigned to a parameter
    if (length(new_value) != 1) {
      createAlert(session,  anchorId = "errorMultipleParameter", alertId = "alertMultipleParameter", title = "Error!",
                content = "You can only assign one value to a parameter.", style = "danger")
      return(NULL)
    }
    
    # Check that the parameter values are within the proper range. Does not apply for a1 or b1
    if (!(input$chooseParameters %in% c("a1","b1"))) {
      if ((new_value < lowerParameters[input$chooseParameters]) | (new_value > upperParameters[input$chooseParameters])) {
        createAlert(session,  anchorId = "errorRangeParameter", alertId = "alertRangeParameter", title = "Error!",
                content = paste0("The value for parameter ",input$chooseParameters, 
                                 " must be between ", lowerParameters[input$chooseParameters], " and ",
                                 upperParameters[input$chooseParameters]), style = "danger")
        return(NULL)
      }
    } else {
      vpd = reactives$allScenarios[[reactives$currentManagedScenario]][["VPD"]]
      if(input$chooseParameters == "a1") {
        a1 = new_value
        b1 = reactives$allScenarios[[reactives$currentManagedScenario]][["b1"]]
      } else {
        b1 = new_value
        a1 = reactives$allScenarios[[reactives$currentManagedScenario]][["a1"]]
      }
      fvpd = 1/(1/(a1 - b1*vpd) - 1)
      if(any(!is.finite(fvpd)) | any(fvpd < 0)) {
        createAlert(session,  anchorId = "errorRangeParameter", alertId = "alertRangeParameter", title = "Error!",
                content = "The value for the parameters a1 and b1 are constrained such that 1/(1/(a1 - b1*vpd) - 1) is positive and finite", style = "danger")
        return(NULL)
      }
    }
    
    reactives$allScenarios[[reactives$currentManagedScenario]][[input$chooseParameters]] = new_value
    
  }) 

  
  # Reset button will restore the parameter to the default values
  observeEvent(input$resetParameters, {
    closeAlert(session,  alertId = "alertStringParameter")
    closeAlert(session,  alertId = "alertMultipleParameter")
    closeAlert(session,  alertId = "alertRangeParameter")
    
    if(reactives$allPhotosType[[reactives$currentManagedScenario]] == "C3") {
      selectedParameter = C3default[[input$chooseParameters]]
    } else {
      selectedParameter = C4default[[input$chooseParameters]]
    }
    updateTextInput(session = session, inputId = "valueParameters", label = NULL, 
                    value =  selectedParameter)
    reactives$allScenarios[[reactives$currentManagedScenario]][[input$chooseParameters]] = selectedParameter
  })  
  
  # Add tooltip with a description of a parameter and its units. The behaviour is inconsistent
  output$parameterDescription = renderUI({
    HTML(descriptionParameters[[input$chooseParameters]])
  })
  
  ################
  # Run scenario #
  ################  
  # Event triggered by running the actual simulation
  observeEvent(input$runScenario, {
    # Perform the simulation
    if(reactives$allPhotosType[[reactives$currentManagedScenario]] == "C3") {
      tryCatch({tempOutput = C3(reactives$allScenarios[[reactives$currentManagedScenario]])}, 
               error = function(e) {reactives$errorSimulation = TRUE})
    } else {
      tryCatch({tempOutput = C4(reactives$allScenarios[[reactives$currentManagedScenario]])}, 
               error = function(e) {reactives$errorSimulation = TRUE})          
    }
    tempOutput = as.data.frame(tempOutput)
    tempOutput = transform(tempOutput, Scenario = reactives$currentManagedScenario, CurveType = reactives$allCurveTypes[[reactives$currentManagedScenario]])
    # Check if the scenario has already been simulated 
    if(reactives$currentManagedScenario %in% unique(reactives$allOutputs$Scenario)) {
      reactives$allOutputs = reactives$allOutputs[-which(reactives$allOutputs$Scenario == reactives$currentManagedScenario),]
      reactives$allOutputs = rbind(reactives$allOutputs,tempOutput)
    } else {
      reactives$allOutputs = rbind(reactives$allOutputs,tempOutput)
    }
    reactives$flagOutputs = reactives$flagOutputs + 1
    
    # Update the checkbox group to add the new scenario and select it
    if("listOutputs" %in% names(input)) {
      updateCheckboxGroupInput(session = session, inputId = "listOutputs", 
                               label = "Choose scenarios", choices = unique(reactives$allOutputs$Scenario), 
                               selected = c(input$listOutputs, reactives$currentManagedScenario))
    }
  })
  
  # Observe for possible errors in the simulation
  observe({
    if (reactives$errorSimulation == TRUE) {
      createAlert(session,  anchorId = "errorRunScenario", alertId = "alertErrorSimulation", title = "Error!",
                  content = "There was an error when calculating the outputs of the scenario", style = "danger")
    } else if (reactives$errorSimulation == FALSE) {
      closeAlert(session,  alertId = "alertErrorSimulation")
    } 
  })  
  
  ##################
  # Plot scenarios #
  ##################
  
  # Update the list of scenarios that can be plotted (i.e. analysedScenarios)
  # Use the CSS class multicol defined in the header to make it multicolumn (the order of filling can be awkward though...)
  output$analysedScenarios <- renderUI({
    req(reactives$flagOutputs > 1, reactives$allOutputs)
    if (!is.null(isolate(input$listOutputs))) {
      return(tags$div(align = 'left', 
                      class = 'multicol',
             checkboxGroupInput(inputId = "listOutputs", label = "Scenarios: ", 
                       unique(reactives$allOutputs$Scenario), 
                      selected = isolate(input$listOutputs))))
    } else {
      return(tags$div(align = 'left', 
                      class = 'multicol',
                      checkboxGroupInput(inputId = "listOutputs", label = "Scenarios: ", 
                      unique(reactives$allOutputs$Scenario), 
                      selected = reactives$currentManagedScenario)))#reactives$allOutputs$Scenario[1])
    }
  })
  
  # Depending on the values of allOutputs, listOutputs and nplots, call the different modules
  observe({
    req(reactives$allOutputs, input$nplots > 0)
    
    localOutput = subset(reactives$allOutputs, Scenario %in% input$listOutputs)

    callModule(module = interactivePlot, id = "plot1", Outputs = localOutput, Description = c(RdescriptionOutput, RdescriptionEnvironment))
    if (input$nplots > 1)
      callModule(module = interactivePlot, id = "plot2", Outputs = localOutput, Description = c(RdescriptionOutput, RdescriptionEnvironment))
    if (input$nplots > 2)
      callModule(module = interactivePlot, id = "plot3", Outputs = localOutput, Description = c(RdescriptionOutput, RdescriptionEnvironment))   
    if (input$nplots > 3)
      callModule(module = interactivePlot, id = "plot4", Outputs = localOutput, Description = c(RdescriptionOutput, RdescriptionEnvironment)) 
    if (input$nplots > 4)
      callModule(module = interactivePlot, id = "plot5", Outputs = localOutput, Description = c(RdescriptionOutput, RdescriptionEnvironment)) 
    if (input$nplots > 5)
      callModule(module = interactivePlot, id = "plot6", Outputs = localOutput, Description = c(RdescriptionOutput, RdescriptionEnvironment))     
  })
})



