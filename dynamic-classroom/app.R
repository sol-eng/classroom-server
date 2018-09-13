library(shiny)
library(magrittr)
library(shinycssloaders)
library(dplyr)
library(DBI)
library(glue)
library(odbc)
library(uuid)
library(shinycookie)

source("helper.R")

cfg <- config::get("database", file = "config.yml", config = "rsconnect")
con <- do.call(pool::dbPool, cfg)

prefix <- "test"
schema <- "classroom"

dbExecute(con, "SET search_path=classroom;")

onStop(fun = function(){
    message("This is onStop firing at the application level")
    pool::poolClose(con)
    })

ui <- fluidPage(
    
    # Application title
    initShinyCookie("cookie"),
    titlePanel("Workshop Classroom"),
    actionButton("back_to_0", "Home"),
    uiOutput("admin_option"),
    uiOutput("page_0"), # %>% withSpinner(),
    uiOutput("page_1"),
    uiOutput("page_2"),
    
    uiOutput("page_10")
)

server <- function(input, output, session) {
    
    # useful objects ---------------------------
    
    classroom <- tbl(
        con, 
        glue("{prefix}classroom")
        )
    
    student <- tbl(
        con,
        glue("{prefix}student")
    )
    
    claim <- tbl(
        con,
        glue("{prefix}claim")
    )
    
    instance <- tbl(
        con,
        glue("{prefix}instance")
    )
    
    active_class <- reactiveVal(value = NULL, label = "selected_class")
    active_student <- reactiveVal(value = NULL, label = "selected_student")
    
    active_cookie <- reactiveVal(value = NULL, label = "current_cookie")
    
    classroom_vector <- reactivePoll(
        5000, 
        session = session
        , checkFunc = function(){
            print("Checking classroom edits")
            dbGetQuery(
                con, 
                glue("SELECT max(lastmodified) FROM {schema}.{prefix}classroom;")
            )
        }
        , valueFunc = function(){
            as.list(set_names(classroom %>% pull(classroomid), classroom %>% pull(name)))
        }
    )
    
    
    # state model -------------------------------
    
    state <- reactiveVal(0, label = "state")
    
    output$admin_option <- renderUI({
        if (req(session$user %in% c("cole") || as.logical(Sys.getenv("ENABLE_ADMIN")))) {
            actionButton("to_admin_page", "To Admin Page")
        }
    })
    observeEvent(input$to_admin_page, {
        state(10);
    })
    
    observeEvent(input$back_to_0, {
        state(0);
    })
    
    
    # state = 0 : prompt for password --------------------------
    output$page_0 <- renderUI({
        req(state() == 0);
        
        log_event(con = con, schema = schema, prefix = prefix, event = "Reached state: 0"
                  , session = session$token)
        
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
          if (selected_record %>% collect() %>% nrow() == 0) {
              state(0)
          }
          # keep just ID, so we have to look things up each time
          # beware deletion of an ID...
          active_class(selected_record %>% pull(classroomid))
          
          active_cookie(input$cookie[[glue("classroom{active_class()}")]])
          
          log_event(con = con, schema = schema, prefix = prefix, event = "Entering classroom"
                    , session = session$token, classroomid = active_class()
                    , cookie = active_cookie())
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
        
        log_event(con, schema, prefix, "Reached state: 1", 
                  session = session$token,
              classroomid = active_class(),
              cookie = active_cookie())
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
        log_event(con, schema, prefix, "Submitted name and email", 
                  session = session$token,
              classroomid = active_class(), 
              cookie = active_cookie(),
              other = glue::glue("Name: {input$name_1}; Email: {input$email_1}")
              )
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
        
        input_email <- input$email_1 %>% 
            stringr::str_to_lower() %>%
            stringr::str_trim()
        curr_student <- student %>% 
            filter(tolower(email) == input_email)
        
        if (curr_student %>% collect() %>% nrow() > 0) {
            # found student
            
            active_student(curr_student %>% pull(studentid))
        
            log_event(con, schema, prefix, event = "Found student", 
                      session = session$token,
                  classroomid = active_class(), studentid = active_student(),
                  cookie = active_cookie())
            set_student_consent(con = con, schema = schema
                                , prefix = prefix
                                , student = active_student()
                                , consent = "true")
            set_student_name(con = con, schema = schema
                             , prefix = prefix
                             , studentid = active_student()
                             , name = input$name_1)
            
            if (is.null(input$cookie[[glue("classroom{active_class()}")]])) {
                active_cookie(UUIDgenerate())
                updateCookie(session,
                         !!!as.list(set_names(active_cookie(), glue("classroom{active_class()}")))
                         )
                set_student_cookie(con, schema = schema, prefix = prefix
                                   , studentid = active_student()
                                   , cookie = active_cookie())
                log_event(con = con, schema = schema, prefix = prefix, event = "Set cookie"
                          , session = session$token
                          , classroomid = active_class()
                          , studentid = active_student()
                          , cookie = active_cookie()
                          )
            } else {
                active_cookie(input$cookie[[glue("classroom{active_class()}")]])
                log_event(con = con, schema = schema, prefix = prefix, event = "Cookie already exists"
                          , session = session$token
                          , classroomid = active_class()
                          , studentid = active_student()
                          , cookie = active_cookie()
                          )
                set_student_cookie(con, schema = schema, prefix = prefix
                                   , studentid = active_student()
                                   , cookie = active_cookie())
            }
            
            state(2)
        } else {
            # did not find student
            log_event(con, schema, prefix, "WARNING: User not found!!", 
                      session = session$token,
                  classroomid = active_class(), 
                  cookie = active_cookie(),
                  other = glue::glue("Name: {input$name_1}; Email: {input$email_1}")
                  )
            showNotification(
                "Your email was not found in the classroom. 
                Please double check spelling and try again"
                , type = "error"
            )
        }
        
    })
    
    observeEvent(input$no_modal_1, {
        #state(0)
        log_event(
            con = con, schema = schema, prefix = prefix, event = "Decline consent"
            , session = session$token, classroomid = active_class(),
            cookie = active_cookie()
        )
        removeModal()
    })
    
    # state = 2 : Classroom information  --------------------------
    output$page_2 <- renderUI({
        req(state() == 2);
        
        log_event(con, schema, prefix, "Reached state: 2", 
                  session = session$token,
                  classroomid = active_class(), studentid = active_student()
                  , cookie = active_cookie())
        
        current_student <- active_student()
        student_claim <- claim %>% filter(studentid == current_student)
        claim_id <- student_claim %>% pull(instanceid)
        
        if (length(claim_id) == 0) {
            log_event(con = con, schema = schema, prefix = prefix, event = "WARNING: No server found",
                      session = session$token, classroomid = active_class(), studentid = active_student()
                      , cookie = active_cookie())
        }
        validate(need(length(claim_id) > 0, 
                      message = "I'm sorry. No server is available at present. We will fix this as soon as we can!!",
                      "no_server"
                      ))
        student_instance <- instance %>% 
            filter(instanceid %in% claim_id) %>%
            collect()
        
        log_event(con, schema, prefix, "Found instance", 
                  session = session$token,
                  classroomid = active_class(), studentid = active_student()
                  , instanceid = student_instance %>% pull(instanceid) %>% .[[1]]
                  , cookie = active_cookie()
                  )
        
        student_url <- student_instance %>% pull(url) %>% .[[1]]
        student_user <- student_instance %>% pull(username) %>% .[[1]]
        student_pass <- student_instance %>% pull(password) %>% .[[1]]
        div(
            h3("Your classroom information is below"),
            a(paste("URL:",student_url)
              ,href=as.character(student_url)
              , target="_blank"),
            p(paste("User:",student_user)),
           p(paste("Password:", student_pass)) 
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

