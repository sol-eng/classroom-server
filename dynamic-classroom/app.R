library(magrittr)
library(shinycssloaders)
library(dplyr)
library(DBI)
library(glue)
library(odbc)
library(uuid)
library(shinycookie)
library(shiny)

source("helper.R")

cfg <- config::get("database", file = "config.yml")
con <- do.call(pool::dbPool, cfg)

prefix <- "v1_"
schema <- "classroom"

dbExecute(con, glue("SET search_path={schema};"))

onStop(fun = function(){
    message("This is onStop firing at the application level")
    pool::poolClose(con)
    })

    classroom <- tbl(con,  glue("{prefix}classroom"))
    
    student <- tbl(con, glue("{prefix}student"))
    
    claim <- tbl(con, glue("{prefix}claim"))
    
    instance <- tbl(con, glue("{prefix}instance"))
    
    event <- tbl(con, glue("{prefix}event"))
    
ui <- fluidPage(
    
    # Application title
    initShinyCookie("cookie"),
    titlePanel("Workshop Classroom"),
    actionButton("back_to_0", "Home"),
    uiOutput("admin_option"),
    uiOutput("page_0"), # %>% withSpinner(),
    uiOutput("page_1"),
    uiOutput("page_2"),
    
    uiOutput("page_10"),
    uiOutput("page_10b")
)

server <- function(input, output, session) {
    message("Starting server")
    # useful objects ---------------------------
    
    
    active_class <- reactiveVal(value = NULL, label = "selected_class")
    active_student <- reactiveVal(value = NULL, label = "selected_student")
    
    active_cookie <- reactiveVal(value = NULL, label = "current_cookie")
    
    refresh <- reactiveVal(value = 0, label = "force_refresh")
    
    message("Starting state model")
    # state model -------------------------------
    
    state <- reactiveVal(0, label = "state")
    safe_logical <- function(input, default = FALSE){
        input <- as.logical(input)
        if (is.na(input) || length(input) == 0){
            return(default)
        } else {
            return(input)
        }
    }
        if (!is.null(session$user) && session$user %in% c("cole")) {
            # only define items in an admin context 
            #(so we do not waste bandwidth on the client / server)
            classroom_vector <- reactivePoll(
                10000, 
                session = session
                , checkFunc = function(){
                    message("Checking for classroom updates")
                    dbGetQuery(
                        con, 
                        glue("SELECT max(lastmodified) FROM {schema}.{prefix}classroom;")
                    )
                }
                , valueFunc = function(){
                    as.list(set_names(classroom %>% pull(classroomid), classroom %>% pull(name)))
                }
            )
            
        }
    
    message("Rendering admin option")
    output$admin_option <- renderUI({
        if (!is.null(session$user) && session$user %in% c("cole")) {
            actionButton("to_admin_page", "To Admin Page")
            }
        })
    observeEvent(input$to_admin_page, {
        state(10);
    })
    
    observeEvent(input$back_to_0, {
        state(0);
    })
    
    
    message("Compiling state 0")
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
          if (selected_record %>% tally() %>% pull(n) == 0) {
              # this should not happen
              warning("Strange state where password matched but did not get a record")
              state(0)
          } else {
              # keep just ID, so we have to look things up each time
              # beware deletion of an ID...
              active_class(selected_record %>% pull(classroomid))
              
              active_cookie(input$cookie[[glue("classroom{active_class()}")]])
              
              log_event(con = con, schema = schema, prefix = prefix, event = "Entering classroom"
                        , session = session$token, classroomid = active_class()
                        , cookie = active_cookie())
              state(1);
          }
        } else {
            log_event(con = con, schema = schema, prefix = prefix, event = "WARNING: Password attempt failed"
                      , session = session$token)
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
        
        if (curr_student %>% tally() %>% pull(n) > 0) {
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
            actionButton("admin_back_to_app", "Back to App"),
            actionButton("force_refresh", "Refresh Events"),
            checkboxInput("show_null_events", "Show Pre-Classroom Events", value = FALSE),
            selectizeInput("admin_class", "Select Class", choices = classroom_vector())
        )
    })
    
    observeEvent(input$force_refresh, {
        refresh(refresh() + 1)
    })
    output$page_10b <- renderUI({
        req(state() == 10);
        
        req(input$admin_class)
        admin_selected_class <- input$admin_class
        admin_null_event <- input$show_null_events
        
        output$event_dt <- DT::renderDataTable(
            event %>%
                filter(classroomid == admin_selected_class || (is.na(classroomid) && admin_null_event)) %>%
                select(eventid, event, session, classroomid, instanceid, studentid, lastmodified, other) %>%
                left_join(student %>% select(studentid, classroomid, name, email)
                          , by = c("classroomid", "studentid")) %>%
                collect() %>%
                filter(refresh() >= 0)
            , server = TRUE
        )
        
        div(
          tabsetPanel(
              tabPanel("Student", {
                  DT::datatable(
                      student %>% 
                          filter(classroomid == admin_selected_class) %>%
                          select(studentid, classroomid, name, email, consent, cookie) %>% 
                          collect()
                  )
              }),
              tabPanel("Instance", {
                  DT::datatable(
                      instance %>%
                          filter(classroomid == admin_selected_class) %>%
                          select(instanceid, classroomid, identifier, url, username, password) %>%
                          collect()
                  )
              }),
              tabPanel("Claim", {
                  DT::datatable(
                      claim %>%
                          filter(classroomid == admin_selected_class) %>%
                          select(classroomid, instanceid, studentid, cookie, info) %>%
                          collect()
                  )
              }),
              tabPanel("Join", {
                  DT::datatable(
                      student %>% filter(classroomid == admin_selected_class) %>%
                          select(studentid, classroomid, name, email) %>%
                          left_join(
                              claim %>% select(classroomid, studentid, instanceid), 
                              by = c("classroomid", "studentid")) %>%
                          left_join(
                              instance %>% select(instanceid, classroomid, identifier, url, username, password)
                              , by = c("classroomid", "instanceid")
                          ) %>%
                          collect()
                  )
              }),
              tabPanel("Unclaimed_Instances", {
                  DT::datatable(
                      instance %>% 
                          select(instanceid, classroomid, identifier, url, username, password) %>%
                          left_join(
                              claim,
                              by = c("classroomid", "instanceid")
                          ) %>%
                          filter(is.na(claimid)) %>%
                          collect()
                  )
              }),
              tabPanel("Events", {
                  DT::dataTableOutput("event_dt")
              })
          )
        )
    })
    
    observeEvent(input$admin_back_to_app, {
        state(0);
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

