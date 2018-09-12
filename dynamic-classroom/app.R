library(shiny)
library(magrittr)
library(shinycssloaders)
library(dplyr)
library(DBI)
library(glue)

cfg <- config::get("database", file = "config.yml", config = "rsconnect")
con <- do.call(pool::dbPool, cfg)

prefix <- "test"

dbExecute(con, "SET search_path=classroom;")

ui <- fluidPage(
    
    # Application title
    titlePanel("Workshop Classroom"),
    uiOutput("admin_option"),
    uiOutput("page_0"), # %>% withSpinner(),
    uiOutput("page_1"),
    uiOutput("page_2"),
    
    uiOutput("page_10")
)

server <- function(input, output, session) {
    
    # helpful functions ------------------------
    
    protect_empty <- function(input, out_obj="null"){
        if (input %in% c("''", "") || is.na(input) || 
            is.null(input) || length(input) == 0) {
            return(out_obj)
        } else {
            return(input)
        }
    }
    add_classroom <- function(con, schema, prefix, name, password, status, descriptiion = NULL) {
        description <- description %>% glue::single_quote() %>% protect_empty()
        dbGetQuery(con, glue::glue(
            "INSERT INTO {schema}.{prefix}classroom
            (name, password, status, description)
            values ('{name}', '{password}','{status}', {description})
            RETURNING *
            ;"
        ))
    }
   
    set_student_name <- function(con, schema, prefix, studentid,  name) {
        name <- name %>% glue::single_quote() %>% protect_empty()
        dbGetQuery(con, glue::glue(
            "UPDATE {schema}.{prefix}student
            SET name = {name}
            WHERE studentid = {studentid}
            RETURNING *
            ;
            "
        ))
    }
    update_student <- function(
        con, schema, prefix, classroomid, studentid,
        name, consent, cookie
    )
    add_student <- function(con, schema, 
                            prefix, classroomid, email, name = NULL, 
                            dryrun = FALSE) {
        name <- name %>% glue::single_quote() %>% protect_empty()
        email <- email %>% glue::single_quote()
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}student
            (classroomid, name, email)
            VALUES ({classroomid}, {name}, {email})
            RETURNING *
            ;"
        )
        if (!dryrun) {
            res <-  dbGetQuery(con, query)
            return(res)
        } else {
            return(query)
        }
    }
    
    log_event <- function(con, schema, prefix, event,
                          classroomid="", instanceid="",
                          studentid="", other="", dryrun=FALSE) {
        event <- glue::single_quote(event) %>% protect_empty()
        other <- glue::single_quote(other) %>% protect_empty()
        classroomid <- classroomid %>% protect_empty()
        instanceid <- instanceid %>% protect_empty()
        studentid <- studentid %>% protect_empty()
        other <- other %>% protect_empty()
        
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}event
            (event, classroomid, instanceid, studentid, other)
            VALUES ({event}, {classroomid}, {instanceid}
            , {studentid}, {other})
            RETURNING *
            ;"
        )
        if (!dryrun) {
            res <- dbGetQuery(con, query)
            return(res)
        } else {
            return(query)
        }
    }
    
    # useful objects ---------------------------
    
    classroom <- tbl(
        con, 
        glue("{prefix}classroom")
        )
    
    student <- tbl(
        con,
        glue("{prefix}student")
    )
    
    active_class <- reactiveVal(value = NULL, label = "selected_class")
    active_student <- reactiveVal(value = NULL, label = "selected_student")
    
    # state model -------------------------------
    
    state <- reactiveVal(0, label = "state")
    
    output$admin_option <- renderUI({
        if (req(session$user %in% c("cole"))) {
            actionButton("to_admin_page", "To Admin Page")
        }
    })
    observeEvent(input$to_admin_page, {
        state(10);
    })
    
    
    # state = 0 : prompt for password --------------------------
    output$page_0 <- renderUI({
        req(state() == 0);
        
        div(
            h2("Welcome")
            , p("To begin, please enter the password that
                your classroom instructor has provided.")
            , textInput("text_0", "Input Classroom Password:")
            , actionButton("submit_0", "Submit")
        )
    })
    
    # check that password matches a classroom
    observeEvent(input$submit_0, {
        if (input$text_0 %in% (classroom %>% pull(password))) {
          selected_record <- classroom %>% filter(password == input$text_0)
          stopifnot(selected_record %>% collect() %>% nrow() > 0)
          
          # keep just ID, so we have to look things up each time
          # beware deletion of an ID...
          active_class(selected_record %>% pull(classroomid))
          state(1);
        } else {
          showNotification(
              "ERROR: This password is invalid. Please try again."
              , type = "error"
              , session = session
          )
        }
    })
    
    # state = 1 : prompt for name and email  --------------------------
    output$page_1 <- renderUI({
        req(state() == 1);
        
        classid <- as.integer(active_class())
        classroom_record <- classroom %>% filter(classroomid == classid)
        class_name <- classroom_record %>% pull(name)
        class_desc <- classroom_record %>% pull(description)
        
        div(
            h2(glue::glue("{class_name}")),
            div(class_desc %>% protect_empty(NULL)),
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
                    p("This application uses cookies to ensure 
                      that you have a good user experience. 
                      Do you give your consent to do so?")
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
    
    # state = 2 : Classroom information  --------------------------
    output$page_2 <- renderUI({
        req(state() == 2);
        
        div(
            h3("Your classroom information is below")
        )
    })
    
    # state = 10 : Admin page -----------------------
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

