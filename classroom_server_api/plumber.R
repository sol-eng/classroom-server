library(plumber)
library(dplyr)
library(DBI)
library(magrittr)

cfg <- config::get("database", file = "config.yml")
con <- do.call(pool::dbPool, cfg)
prefix <- "v1_"
schema <- "classroom"
dbExecute(con, glue("SET search_path={schema};"))

#* @apiTitle Classroom Server API

# Classrooms ------

#* Get list of classrooms
#* @get /classrooms
function(req, res, ...) {
  get("classroom")
}



#* Create a classroom
#* @post /create_class
function(name, password, description) {
  db_insert_into(con, class_table("classroom"), 
                 tibble::tibble(name = name, 
                                password = password, 
                                description = description))
}

#* Change Classroom Status
#* @param class_id classroomid from classroom table
#* @param status either ACTIVE or ARCHIVE
#* @param ... 
#* @post /classroom_status
function(req, res, class_id, status, ...) {
  # Safe inputs
  class_id <- as.numeric(class_id)
  stopifnot(status %in% c("ACTIVE", "ARCHIVE"))
  
  # Can't reactivate archived class
  curr_status <- get_classroom_status(class_id)
  if (status == "ACTIVE" && curr_status == "ARCHIVE") {
    # TODO: figure out how to return informative error
    msg <- "Cannot reactivate archived class."
    res$status <- 400
    return(list(error = jsonlite::unbox(msg)))
  }
  
  # suffix name, password, and description
  if (status == "ARCHIVE") archive_class(class_id)
  
  query <- glue::glue(
    "UPDATE {class_table('classroom')}
      SET status = '{status}'
      WHERE classroomid = {class_id}
    RETURNING *;"
  )
  
  dbGetQuery(con, query)
}

# Students -----

#* Get student table
#* @param class_id classroom id from class table, defaults to NULL (all classrooms)
#* @get /student
function(class_id = NULL) {
  df <- get("student")
  
  if (!is.null(class_id)) df <- dplyr::filter(df, 
                                              classroomid == as.numeric(class_id)) 
  df
}

#* Add a student
#* @param class_id classroomid from class table
#* @param name full student name
#* @param email student email
#* @param other other details, defaults to NULL
#* @post /student
function(req, res, class_id, name, email, other = NULL) {
  class_id <- as.numeric(class_id)
  name <- safe_chr(name)
  email <- safe_chr(email)
  other <- safe_chr(other)
  
  if (!class_id %in% get("classroom")$classroomid) {
    msg <- "Invalid class id."
    res$status <- 400
    return(list(error = jsonlite::unbox(msg)))
  }
  
  query <- glue::glue(
    "INSERT INTO {class_table('student')}
      (classroomid, name, email, other)
      VALUES ({class_id}, {name}, {email}, {other})
      RETURNING *;"
  )
  dbGetQuery(con, query)
}

#* Update a student attribute
#* @param class_id classroom id
#* @param student_id student id
#* @param attribute attribute, one of name, email, consent, cookie, other
#* @param value value to update
#* @post /update_student
function(class_id, student_id, attribute, value) {
  class_id <- as.numeric(class_id)
  student_id <- as.numeric(student_id)
  value <- safe_chr(value)
  
  stopifnot(attribute %in% c("name", "email", "consent", "cookie", "other"))
  
  query <- glue::glue(
    "UPDATE {class_table('student')}
      SET {attribute} = {value}
      WHERE classroomid = {class_id} AND studentid = {student_id}
    RETURNING *;"
  )
  dbGetQuery(con, query)
}

# Instances ------

#* Get Instances
#* @param class_id id of classroom
#* @param unclaimed only unclaimed instances? defaults to FALSE
#* @get /instance
function(class_id = NULL, unclaimed = FALSE) {
  df <- get("instance")
  
  if (!is.null(class_id)) df <- dplyr::filter(df, classroomid == class_id)
  
  # TODO: Make unclaimed work
  
  df
}

#* Add instance
#* @param class_id classroom id
#* @param identifier instance identifier
#* @param url instance URL
#* @param username instance username
#* @param password instance password
#* @post /instance
function(req, res, class_id, identifier, url, username, password) {
  class_id <- as.numeric(class_id)
  identifier %<>% safe_chr()
  url %<>% safe_chr()
  username %<>% safe_chr()
  password %<>% safe_chr()
  
  if (!class_id %in% get("classroom")$classroomid) {
    msg <- "Invalid class id."
    res$status <- 400
    return(list(error = jsonlite::unbox(msg)))
  }
  
  query <- glue::glue(
    "INSERT INTO {class_table('instance')}
      (classroomid, identifier, url, username, password)
      VALUES ({class_id}, {identifier}, {url}, {username}, {password})
      RETURNING *;"
  )
    
    dbGetQuery(con, query)
}

# TODO: Allow calling with list of instances?
# TODO: Allow update to instance attributes
# TODO: Allow claiming instances
# TODO: Add event logging.