library(shiny)
library(tidyverse)
library(DBI)
library(odbc)

type_of_info <- config::get("info")

frame <- tibble::tribble(
  ~ "id", ~ "url", ~"user_name", ~"password", ~"claim_count", ~"exclude"
  , 0, "http://something", "my_username", "my_password", 0, FALSE
) %>% filter(FALSE)

if (type_of_info == "database") {
  db_info <- config::get("database")
  con <- do.call(pool::dbPool, c(db_info, list(validateQuery = "select 1")))
  
  # ensure existence
  if (!dbExistsTable(con, db_info$table)) {
    dbWriteTable(con, db_info$table, frame)
  }
  
  instances <- tbl(con, db_info$table)
} else {
  print("Using a local storage!!")
  instances <- read_csv("classroom_instances.csv", col_names = FALSE) %>%
    rename(
      url = X1,
      user_name = X2,
      password = X3
    ) %>%
    mutate(id = row_number())
  
  instances <- bind_rows(frame, instances)
}

  update_claim <- function(input_data, select_id) {
    if (inherits(input_data, "tbl_sql")) {
      # update database
      dbExecute(input_data[[1]]$con, 
                paste("UPDATE"
                       , input_data[[2]]$x %>% as.character()
                       , "SET claim_count = claim_count + 1"
                       , "WHERE id ="
                       , select_id
                       , ";")
                )
      return(input_data)
    } else {
      # update local table
      return(input_data %>% mutate(claim_count = if_else(id == select_id, claim_count + 1, claim_count)))
    }
  }
  get_instance <- function(input_data, select_id) {
    validate(need(select_id, message = "Invalid ID", label = "id_check"))
    sel_data <- input_data %>%
      filter(id == select_id)
    
    pre_claim_count <- sel_data %>% pull(claim_count)
    pre_claim_count <- ifelse(length(pre_claim_count)==0, 0, pre_claim_count)
    
    # check pre-claimed
    if (pre_claim_count > 0) {
      shiny::showNotification(
        paste("WARNING: Server has been claimed"
                , pre_claim_count
                , "times... be certain this is your server!")
        , duration = 10
        , closeButton = TRUE
        , type = "warning"
      )
    }
    
    update_claim(input_data = input_data, select_id = select_id)
    
    output <- sel_data %>% collect()
    
    # if no output
    if (nrow(output) == 0) {
      shiny::showNotification(
        paste("ERROR: no server available for ID =", select_id)
        , duration = 10
        , type = "error"
      )
    }
    return(output %>% as.list())
  }


ui <- fluidPage(theme = "rstudio.css",

  titlePanel("Casualty Actuarial Society 2018"),
  

  sidebarLayout(
    sidebarPanel(
      textInput("number", "Assigned server number", width = "300px"),
      actionButton("submit","Submit", width = "100px"),
      htmlOutput("url"),
      textOutput("user"),
      textOutput("password"),
      width = 4
    ),
    mainPanel(

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  

  observeEvent(input$submit
               , {
                 curr_instance <- get_instance(
                   instances
                   , as.numeric(input$number)
                   )
                 output$url <- renderUI({
                   geturl <- curr_instance[["url"]]
                   validate(need(geturl, "ERROR: There is no such server", "url"))
                   url <- a(paste("URL:",geturl)
                              ,href=as.character(geturl)
                              , target="_blank")
                   return(url)
                 })
                 output$user <- renderText({
                   username <- curr_instance[["user_name"]]
                   validate(need(username, message = FALSE, "username"))
                   return(paste("User:",username))
                 })
                 output$password <- renderText({
                   password <- curr_instance[["password"]]
                   validate(need(password, message = FALSE, "password"))
                   return(paste("Password:",password))
                 })
                 
               })
 
}

# Run the application
shinyApp(ui = ui, server = server)