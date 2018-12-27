simpleUI <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    tags$p("blah")
  )
}

# Module server function
simpleServer <- function(input, output, session) {
  print("blah")
}

testModule <- function(uiInput, serverInput, ...) {
  library(shiny)
  
  ui <- fluidPage(
        uiInput
  )
  
  server <- function(input, output, session) {
    datafile <- callModule(serverInput, ...)
  }
  
  shinyApp(ui, server)
}

testModule(simpleUI("datafile"), simpleServer, "datafile")

testModule <- function(
  id, 
  ui, 
  server, 
  ui_param = list(), 
  server_param = list()
  ){
  
  actualUI <- do.call(ui, c(id = id, ui_param))
  
  actualServer <- function(input, output, session) {
    do.call(callModule,
            c(module = server
              , id = id
              , server_param
              )
            )
  }
  
  shinyApp(actualUI, actualServer)
}

#testModule("datafile", simpleUI, simpleServer)
