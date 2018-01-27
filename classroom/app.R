library(shiny)
library(tidyverse)

instances <- read_csv("classroom_instances.csv", col_names = FALSE) %>%
  rename(
    url = X1,
    user_name = X2,
    password = X3
  )

ui <- fluidPage(theme = "rstudio.css",

  titlePanel("Big Data with R"),
  

  sidebarLayout(
    sidebarPanel(
      textInput("number", "Assigned server number"),
      textOutput("url"),
      textOutput("user"),
      textOutput("password")
    ),
    mainPanel(

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$user <- renderText({
    getuser <- instances %>%
      rownames_to_column() %>%
      filter(rowname == input$number) %>%
      select(user_name)

    username <- ifelse(nrow(getuser) == 0, "User Name", as.character(getuser))
  })

  output$password <- renderText({
    getpass <- instances %>%
      rownames_to_column() %>%
      filter(rowname == input$number) %>%
      select(password)

    password <- ifelse(nrow(getpass) == 0, "Password", as.character(getpass))
  })

  output$url <- renderText({
    geturl <- instances %>%
      rownames_to_column() %>%
      filter(rowname == input$number) %>%
      select(url)

    url <- ifelse(nrow(geturl) == 0, "URL", as.character(geturl))
  })
}

# Run the application
shinyApp(ui = ui, server = server)