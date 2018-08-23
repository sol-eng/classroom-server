library(shiny)
library(magrittr)
library(shinycssloaders)

ui <- fluidPage(
    
    # Application title
    titlePanel("Old Faithful Geyser Data"),
    uiOutput("admin_option"),
    uiOutput("page_0") %>% withSpinner(),
    uiOutput("page_1"),
    uiOutput("page_2"),
    
    uiOutput("page_10")
)

server <- function(input, output, session) {
    
    state <- reactiveVal(0, label = "state")
    
    output$admin_option <- renderUI({
        if (session$user %in% c("cole")) {
            actionButton("to_admin_page", "To Admin Page")
        }
    })
    observeEvent(input$to_admin_page, {
        state(10);
    })
    
    
    # state = 0 : prompt for password
    output$page_0 <- renderUI({
        req(state() == 0);
        
        div(
            h2("Hello")
            , p("What is going on?")
            , textInput("text_0", "Input Classroom Password:")
            , actionButton("submit_0", "Submit")
        )
    })
    
    # check that password matches a classroom
    observeEvent(input$submit_0, {
        state(1);
    })
    
    # state = 1 : prompt for name and email
    output$page_1 <- renderUI({
        req(state() == 1);
        
        div(
            h2("Welcome to the classroom!"),
            p("Please enter your name and email address"),
            textInput("name_1", "Name: "),
            textInput("email_1", "Email: "),
            actionButton("submit_1", "Submit")
        )
        
    })
    
    observeEvent(input$submit_1, {
        showModal(
            modalDialog(
                div(
                    p("This application uses cookies to ensure that you have a good user experience. Do you give your consent to do so?")
                )
                , title = "Your Information"
                , footer = div(actionButton("no_modal_1", "No"), actionButton("yes_modal_1", "Yes"))
                )
            )
        
    })
        
    
    observeEvent(input$yes_modal_1, {
        removeModal()
        state(2)
    })
    
    observeEvent(input$no_modal_1, {
        state(0)
        removeModal()
    })
    
    output$page_2 <- renderUI({
        req(state() == 2);
        
        div(
            h3("Your classroom information is below")
        )
    })
    
    
    output$page_10 <- renderUI({
        req(state() == 10);
        
        div(
            h3("Admin page!"),
            actionButton("admin_back_to_app", "Back to App")
        )
    })
    
    observeEvent(input$admin_back_to_app, {
        state(0);
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

