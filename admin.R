library(shiny)
library(shinyjs)
library(tidyverse)
library(DBI)

type_of_info <- config::get("info", file = "classroom/config.yml")

frame <- tibble::tribble(
  ~ "id", ~ "url", ~"user_name", ~"password", ~"claim_count", ~"exclude"
  , 0, "http://something", "my_username", "my_password", 0, FALSE
) %>% filter(FALSE)

if (type_of_info == "database") {
  db_info <- config::get("database", file = "classroom/config.yml")
  con <- do.call(pool::dbPool, c(db_info, list(validateQuery = "select 1")))
  
  # ensure existence
  if (!dbExistsTable(con, db_info$table)) {
    dbWriteTable(con, db_info$table, frame)
  }
  
  instances <- tbl(con, db_info$table)
} else {
  print("Using a local storage!!")
  instances <- read_csv("classroom/classroom_instances.csv", col_names = FALSE) %>%
    rename(
      url = X1,
      user_name = X2,
      password = X3
    ) %>%
    mutate(id = row_number())
  
  instances <- bind_rows(frame, instances)
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Admin Panel for Classroom Server!!"),
  

  sidebarLayout(
    sidebarPanel(
      width = 4
      , fileInput("csvUpload", "Upload New Data")
      , actionButton("uploadData", "Confirm and Submit New Data")
    ),
    mainPanel(
      shiny::fluidRow(h3("Actual Data:"),dataTableOutput("rawdata"))
      , shiny::fluidRow(h3("New Data:"),dataTableOutput("newdata")) 
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  rval <- reactiveVal(1, label = "Test")
  new_data <- reactiveVal(frame)
  all_data <- reactivePoll(intervalMillis = 1000
               , session = session
               , checkFunc = function(){instances %>% group_by(1) %>% summarize(count=n()) %>% pull(count)}
               , valueFunc = function(){instances %>% collect()})
 
  
  output$rawdata <- renderDataTable({
    all_data()
  })
  
  observeEvent(input$csvUpload, {
    new_data({
      validate(need(input$csvUpload, message=FALSE, label="CSV Upload Check"))
      print(str(input$csvUpload))
      file_path <- input$csvUpload[["datapath"]]
      parsed <- read_csv(file = file_path, col_names = FALSE) %>%
        rename(
          url = X1,
          user_name = X2,
          password = X3
        )
      bind_rows(frame, parsed) %>%
        mutate(id = row_number() + max(instances %>% pull(id)) %>% if_else(length(.)==0,0,.))})
  })

  output$newdata <- renderDataTable({
    new_data()
  })
  
  
  observeEvent(input$uploadData, {
    
    # set defaults
    modified <- new_data() %>%
      mutate(
        claim_count = 0
        , exclude = FALSE
      )
    
    # update new data
    if(inherits(instances, "tbl_sql")) {
      dbWriteTable(
        conn = instances[[1]]$con
        , name = instances[[2]]$x
        , value = modified
        , append = TRUE
      )
      shiny::showNotification(
        "Data uploaded successfully"
        , type = "message"
      )
    } else {
      instances <- bind_rows(
        instances, modified
      )
    }
    
    # clear new_data
    reset("csvUpload")
    new_data(frame)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)