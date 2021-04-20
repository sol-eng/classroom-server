library(magrittr)
library(shinycssloaders)
library(dplyr)
library(DBI)
library(glue)
library(odbc)
library(uuid)
library(shinycookie)
library(shiny)
library(shinyWidgets)

source("helper.R")

if (Sys.getenv("R_CONFIG_ACTIVE") %in% c("rsconnect", "colorado")) {
  cfg <- list(
    drv = odbc::odbc(),
    Driver = "PostgreSQL",
    Host = Sys.getenv("CLASSROOM_DB_HOST"),
    Database = Sys.getenv("CLASSROOM_DB_NAME"),
    UID = Sys.getenv("CLASSROOM_DB_USER"),
    PWD = Sys.getenv("CLASSROOM_DB_PASSWORD")
  )
} else {
  cfg <- config::get("database", file = "config.yml")
}
con <- do.call(pool::dbPool, cfg)


prefix <- "v1_"
schema <- "classroom"

dbExecute(con, glue("SET search_path={schema};"))

onStop(fun = function(){
  message("This is onStop firing at the application level")
  pool::poolClose(con)
})

classroom <- tbl(con,  glue("{prefix}classroom")) %>% filter(status == 'ACTIVE')

student <- tbl(con, glue("{prefix}student"))

claim <- tbl(con, glue("{prefix}claim"))

instance <- tbl(con, glue("{prefix}instance"))

event <- tbl(con, glue("{prefix}event"))

ui <- htmlTemplate(
  "www/index.htm",
  init_cookie = initShinyCookie("cookie"),
  home_button = actionLink("back_to_0", "Home"),
  page_0 = uiOutput("page_0"), # %>% withSpinner(),
  page_1 = uiOutput("page_1"),
  page_2 = uiOutput("page_2"),
  page_10 = uiOutput("page_10"),
  page_10b = uiOutput("page_10b")
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
  if (is_admin(session$user)) {
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

  if (is_admin(session$user)) {
    message("Rendering admin option")
    insertUI(
      "#navbar > ul",where = "beforeEnd", ui = tags$li(
        actionLink("to_admin_page", "Admin Page")
      )
    )
  }

  # important, as a user could otherwise hijack this with JS
  if (is_admin(session$user)){
    observeEvent(input$to_admin_page, {
      state(10);
    })
  }

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
      tags$br(),
      textInput("text_0", "Workshop identifier"),
      actionButton("submit_0", "Submit")
    )

  })

  # check that password matches a classroom
  observeEvent(input$submit_0, {
    if (input$text_0 %in% (classroom %>% pull(password))) {
      selected_record <- classroom %>% filter(password == !!input$text_0)
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
      log_event(con = con, schema = schema, prefix = prefix,
                event = "WARNING: Password attempt failed",
                other = glue::glue("Password: {input$text_0}"),
                session = session$token)
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

    if (!is.null(active_cookie())) {
      # get student from cookie
      classid <-  as.integer(active_class())
      user_cookie <- active_cookie()
      curr_student <- student %>%
        filter(classroomid == classid, cookie == user_cookie)
      
      
      if (curr_student %>% tally() %>% pull(n) > 0) {
        # found student
        log_event(con, schema, prefix, event = "Found student based on cookie", 
                  session = session$token,
                  classroomid = active_class(), studentid = active_student(),
                  cookie = active_cookie())
        active_student(curr_student %>% pull(studentid) %>% .[[1]])
        state(2)
      } else {
        # student not found
        log_event(con, schema, prefix, event = "WARNING: Could not find student based on cookie", 
                  session = session$token,
                  classroomid = active_class(), studentid = active_student(),
                  cookie = active_cookie())
        
        showNotification(
          "Sorry, we could not find your instance based on the cookie in your browser.
                  Please enter your identifying information"
          , type = "error"
        )
      }
    }

    div(
      h2(glue::glue("{class_name}")),
      div(class_desc %>% protect_empty(NULL) %>% HTML()),
      textInput("name_1", "Full Name"),
      textInput("email_1", "Email"),
      materialSwitch(inputId = "here_before_1",
                     label = textOutput("here_before_1_label"),
                     status = "success", value = TRUE),
      actionButton("submit_1", "Submit")
    )

  })

  output$here_before_1_label <- renderText(ifelse(!input$here_before_1,
                                                  "Look up previous server credentials",
                                                  "Create new server credentials"))

  observeEvent(input$submit_1, {
    log_event(con, schema, prefix, "Submitted name and email",
              session = session$token,
              classroomid = active_class(),
              cookie = active_cookie(),
              other = glue::glue("Name: {input$name_1}; Email: {input$email_1}")
    )
    
    # validate that email input is actually valid
    # thanks to: https://emailregex.com/
    email_regex <- "(^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$)"
    if (is.null(input$email_1) || !stringr::str_detect(input$email_1, email_regex)) {
      showNotification(
        "Invalid 'Email' input. Please try again!", 
        type = "error"
      )
    } else {
      input_email <- input$email_1 %>% 
        stringr::str_to_lower() %>%
        stringr::str_trim()
      # lookup of the current student by email
      classid <- as.integer(active_class())
      curr_student <- student %>%
        filter(classroomid == classid, tolower(email) == input_email)
  
      if (curr_student %>% tally() %>% pull(n) > 0 &&
          curr_student %>% inner_join(claim, by = c("studentid","classroomid")) %>% tally() %>% pull(n) > 0
      ) {
        # found student and claim
  
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
  
        # set cookie
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
      } else if (
        input$here_before_1 ||
        (curr_student %>% tally() %>% pull(n) > 0 &&
         curr_student %>% inner_join(claim, by = c("studentid","classroomid")) %>% tally() %>% pull(n) == 0
        )
      ) {
        # did not find student/instance, new student
        class_status <- classroom %>% filter(classroomid == classid) %>% pull(status)
        if (class_status != "ACTIVE") {
          # new students not welcome...
          log_event(
            con, schema, prefix,
            glue::glue("WARNING: Tried to claim new instance with classroom state: {class_status}"),
            session=session$token,
            classroomid = active_class(),
            cookie = active_cookie(),
            other = glue::glue("Name: {input$name_1}; Email: {input$email_1}")
          )
          showNotification(
            "Sorry, new instances are not available at this time. Please contact your TA",
            type = "error"
          )
        } else {
          # welcome, new student!
          log_event(
            con, schema, prefix,
            "Add student and Claim instance",
            session=session$token,
            classroomid = active_class(),
            cookie = active_cookie(),
            other = glue::glue("Name: {input$name_1}; Email: {input$email_1}")
          )
          if (curr_student %>% tally() %>% pull(n) == 0) {
            # add student record
            new_student <- add_student(
              con = con, schema = schema, prefix = prefix, classroomid = classid,
              email = input$email_1, name = input$name_1
            )
            active_student(new_student %>% pull(studentid))
          } else {
            # existing student
            active_student(curr_student %>% pull(studentid))
  
            log_event(con, schema, prefix, event = "Found student",
                      session = session$token,
                      classroomid = active_class(), studentid = active_student(),
                      cookie = active_cookie())
            set_student_name(con = con, schema = schema
                             , prefix = prefix
                             , studentid = active_student()
                             , name = input$name_1)
          }
          set_student_consent(con = con, schema = schema
                              , prefix = prefix
                              , student = active_student()
                              , consent = "true")
  
          # set cookie
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
  
          # claim available instance
          claim_instance <- add_claim(
            con = con,
            schema = schema,
            prefix = prefix,
            classroomid = active_class(),
            studentid = active_student()
          )
          if (claim_instance %>% tally() %>% pull(n) == 1) {
            # successful claim
            log_event(con, schema, prefix, event = "Successful instance claim",
                      session = session$token,
                      classroomid = active_class(), studentid = active_student(),
                      instanceid = claim_instance %>% pull(instanceid),
                      cookie = active_cookie())
  
            state(2)
          } else {
            # unsuccessful claim
            log_event(con, schema, prefix, event = "WARNING: Failed to claim instance",
                      session = session$token,
                      classroomid = active_class(), studentid = active_student(),
                      cookie = active_cookie())
  
            showNotification(
              "Sorry, an instance is not available at this time. Please contact your TA.",
              type = "error"
            )
          }
        }
      } else {
        # did not find student, and is a returning user
        log_event(con, schema, prefix, "WARNING: User not found!!",
                  session = session$token,
                  classroomid = active_class(),
                  cookie = active_cookie(),
                  other = glue::glue("Name: {input$name_1}; Email: {input$email_1}")
        )
        showNotification(
          "Sorry, your email was not found in the classroom.
                  Please double check spelling and try again. Otherwise,
                  you can contact a TA for assistance."
          , type = "error"
        )
      }
      
    }
    
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
      # clear cookie when no classroom found
      # TODO: Find a way to _keep_ the cookie, but still allow reclaiming
      # that way the user does not have to login again...
      removeCookie(glue("classroom{active_class()}"))
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

    fluidPage(
      h3("Admin page"),
      actionButton("admin_back_to_app", "Back to App"),
      actionButton("force_refresh", "Refresh Events"),
      checkboxInput("show_null_events", "Show Pre-Classroom Events", value = FALSE),
      selectizeInput("admin_class", "Select Class", choices = classroom_vector()),
      actionButton("create_class", "Create New Classroom"),
      actionButton("upload_instances", "Upload Instances")
    )
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
    admin_student_data <- reactivePoll(
      intervalMillis = 10000,
      session = session,
      checkFunc = function(){
        student %>%
          filter(classroomid == admin_selected_class) %>%
          tally() %>% pull(n)
      },
      valueFunc = function(){
        req(admin_selected_class)
        student %>%
          filter(classroomid == admin_selected_class) %>%
          select(studentid, classroomid, name, email, consent, cookie) %>%
          collect()
      }
    )
    output$admin_student_dt <- DT::renderDataTable(
      admin_student_data() %>% DT::datatable()
    )

    admin_instance_data <- reactivePoll(
      intervalMillis = 10000,
      session = session,
      checkFunc = function(){
        instance %>%
          filter(classroomid == admin_selected_class) %>%
          tally() %>% pull(n)
      },
      valueFunc = function() {
        req(admin_selected_class)
        instance %>%
          filter(classroomid == admin_selected_class) %>%
          select(instanceid, classroomid, identifier, url, username, password) %>%
          collect()
      }
    )
    output$admin_instance_dt <- DT::renderDataTable({
      admin_instance_data() %>% DT::datatable()
    })

    admin_unclaimed_instance_data <- reactivePoll(
      intervalMillis = 10000,
      session = session,
      checkFunc = function(){
        instance %>%
          filter(classroomid == admin_selected_class) %>%
          anti_join(
            claim,
            by = c("classroomid", "instanceid")
          ) %>%
          tally() %>% pull(n)
      },
      valueFunc = function() {
        req(admin_selected_class)
        instance %>%
          filter(classroomid == admin_selected_class) %>%
          select(instanceid, classroomid, identifier, url, username, password) %>%
          anti_join(
            claim,
            by = c("classroomid", "instanceid")
          ) %>%
          collect()
      }
    )
    output$admin_unclaimed_instance_dt <- DT::renderDataTable({
      admin_unclaimed_instance_data() %>% DT::datatable()
    })

    admin_claim_data <- reactivePoll(
      intervalMillis = 10000,
      session = session,
      checkFunc = function(){
        claim %>%
          filter(classroomid == admin_selected_class) %>%
          tally() %>%
          pull(n)
      },
      valueFunc = function(){
        claim %>%
          filter(classroomid == admin_selected_class) %>%
          select(classroomid, instanceid, studentid, cookie, info) %>%
          collect()
      }
    )
    output$admin_claim_dt <- DT::renderDataTable({
      admin_claim_data() %>% DT::datatable()
    })
    admin_join_data <- reactivePoll(
      intervalMillis = 10000,
      session = session,
      checkFunc = function(){
        list(
          claim %>%
            filter(classroomid == admin_selected_class) %>%
            summarize(out=max(claimid, na.rm = TRUE)) %>%
            pull(out),
          student %>%
            filter(classroomid == admin_selected_class) %>%
            summarize(out=max(studentid, na.rm = TRUE)) %>%
            pull(out),
          instance %>%
            filter(classroomid == admin_selected_class) %>%
            summarize(out=max(instanceid, na.rm = TRUE)) %>%
            pull(out)
        )
      },
      valueFunc = function() {
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
      }
    )

    output$admin_join_dt <- DT::renderDataTable({
      admin_join_data() %>% DT::datatable()
    })


    div(
      h3("Tables"),
      tabsetPanel(
        tabPanel("Student", {
          DT::dataTableOutput("admin_student_dt")
        }),
        tabPanel("Instance", {
          DT::dataTableOutput("admin_instance_dt")
        }),
        tabPanel("Claim", {
          DT::dataTableOutput("admin_claim_dt")
        }),
        tabPanel("Join", {
          DT::dataTableOutput("admin_join_dt")
        }),
        tabPanel("Unclaimed_Instances", {
          DT::dataTableOutput("admin_unclaimed_instance_dt")
        }),
        tabPanel("Events", {
          DT::dataTableOutput("event_dt")
        })
      )
    )
  })

  # admin-supportive-functionality -----------------------
  if (is_admin(session$user)) {
      output$admin_selected_class <- renderText(classroom %>%
                                                  filter(classroomid == input$admin_class) %>%
                                                  pull(name))
      observeEvent(input$upload_instances, {
        showModal(
          modalDialog(
            div(
              p("Upload instances for: ", textOutput("admin_selected_class")),

              fileInput("new_instance_file", label = "Upload File", multiple = FALSE),
              checkboxInput("new_instance_heading", label = "Heading?", value = FALSE),
              uiOutput("new_instance_select_colnames"),
              DT::dataTableOutput("display_new_instance_data")
            )
            , title = "Upload instance data"
            , footer = div(actionButton("new_instance_cancel", "Cancel"), actionButton("new_instance_submit", "Submit"))
            , size = "l"
          )
        )
      })
      observeEvent(input$new_instance_cancel, {removeModal()})
      observeEvent(input$new_instance_submit, {
        removeModal()
        showNotification(
          "Submitting instance data to the database", type = "message"
        )
        dbWriteTable(
          conn = con,
          name = Id(schema = schema, table = glue::glue("{prefix}instance")),
          value = prep_instance_data(),
          overwrite = FALSE,
          append = TRUE
        )
        showNotification(
          "Finished submitting instance data to the database", type = "message"
        )
      })
      new_instance_file <- reactive({
        message("Executing new_instance_file reactive")

        validate(need(input$new_instance_file, message = FALSE))
        input$new_instance_file
      })
      new_instance_data <- reactive({
        raw_data <- readr::read_csv(
          new_instance_file()$datapath,
          col_names = input$new_instance_heading
        )

        return(raw_data)
      })
      new_instance_colnames <- reactive({
        c("Choose a column" = "", colnames(new_instance_data()))
      })
      output$new_instance_select_colnames <- renderUI({
        req(new_instance_colnames())
        div(
          column(
            6,
            selectizeInput("new_instance_identifier", label = "Identifier", choices = new_instance_colnames(), multiple = FALSE),
            selectizeInput("new_instance_url", label = "Url", choices = new_instance_colnames(), multiple = FALSE)
          ),
          column(
            6,
            selectizeInput("new_instance_username", label = "Username", choices = new_instance_colnames(), multiple = FALSE),
            selectizeInput("new_instance_password", label = "Password", choices = new_instance_colnames(), multiple = FALSE)
          )
        )
      })
      prep_instance_data <- reactive({
        sel_list <- c(
          identifier = safe_name(input$new_instance_identifier),
          url = safe_name(input$new_instance_url),
          username = safe_name(input$new_instance_username),
          password = safe_name(input$new_instance_password)
        )
        new_instance_data() %>%
          mutate(
            !!!sel_list,
            classroomid = input$admin_class
          ) %>%
          select(!!!names(sel_list), classroomid)
      })
      output$display_new_instance_data <- DT::renderDataTable({
        DT::datatable(prep_instance_data())
      })
      observeEvent(input$create_class, {
        showModal(
          modalDialog(
            div(
              p("Enter classroom information"),
              textInput("new_class_name", "Name"),
              textInput("new_class_password", "Class Password"),
              textAreaInput("new_class_description", "Description")
            )
            , title = "New classroom"
            , footer = div(actionButton("create_class_cancel", "Cancel"), actionButton("create_class_submit", "Submit"))
          )
        )
      })
      observeEvent(input$create_class_cancel, {removeModal()})
      observeEvent(input$create_class_submit, {
        new_class <- add_classroom(
          con = con, schema = schema, prefix = prefix,
          name = input$new_class_name,
          password = input$new_class_password,
          status = "ACTIVE",
          description = input$new_class_description
        )
        if (nrow(new_class) > 0) {
          showNotification(
            glue::glue("Successfully created classroom: {new_class$classroomid}"),
            type = "message"
          )
        } else {
          showNotification(
            "Something went wrong creating the classroom...",
            type = "warning"
          )
        }
        removeModal()
      })

      observeEvent(input$force_refresh, {
        refresh(refresh() + 1)
      })

  }

  # other helpers ---------------------------------
  observeEvent(input$admin_back_to_app, {
    state(0);
  })
}

# Run the application
shinyApp(ui = ui, server = server)
