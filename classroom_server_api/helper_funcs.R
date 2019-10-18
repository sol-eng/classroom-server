# Various Helpers for Classroom Management API
library(plumber)
library(dplyr)
library(DBI)
library(magrittr)

# Table Access ------

tables <- function() c("classroom", "event", "instance", "student", "claim")

table_valid <- function(which) {
  which %in% tables()
}

# get table name
class_table <- function(which) {
  stopifnot(table_valid(which))
  
  paste0(prefix, which)
}

# get complete  tables
get <- function(which) {
  stopifnot(table_valid(which))
  dbReadTable(con, class_table(which))
}

# Error handlers ----

# Try, if err Update response status and return
check_err <- function(res, ...){
  tryCatch(..., 
           error = function(e) {
             res$status <- 400
             handle_err(e)
           })
}

# format error for return in res body
handle_err <- function(e) {
  list(error = jsonlite::unbox(e$message))
}

# Protect input -----

# cast empty to null
protect_empty <- function(input, out_obj = "null"){
  if (input %in% c("''", "") || is.na(input) || 
      is.null(input) || length(input) == 0) {
    return(out_obj)
  } else {
    return(input)
  }
}

# Make character strings safe
safe_chr <- function(x) {
  x %>% glue::single_quote() %>% protect_empty()
}

# Check URL well-formed
check_url <- function(x) {
  if(!stringr::str_detect(x, "^http(s)?:\\/\\/.+.com")) {
    stop("URL malformed.")
  }
  NULL
}

# Identify and get objects ----

# Turn object IDs into character string for SQL
make_id_str <- function(...) {
  id_var <- names(list(...))
  id <- c(...) %>% as.numeric()
  
  if(any(is.na(id))) stop("Only numeric IDs are valid.")
  
  paste(glue::glue("{id_var} = {id}"), collapse = " AND ")
}

# check that particular object exists
exists <- function(what, ...) {
  id_str <- make_id_str(...)
  query <- glue::glue(
    "SELECT COUNT(*)
      FROM {class_table(what)}
      WHERE {id_str};"
  )
  
  n <- dbGetQuery(con, query)
  
  if (n == 0) stop("Invalid IDs, no objects found.")
  if (n > 1) stop("Multiple objects with that ID set.")
  
  NULL
}

# create object in table
create_object <- function(which, ...) {
  fields <- names(list(...)) %>% paste(collapse = ", ")
  values <- c(...) %>% safe_chr() %>% paste(collapse = ", ")
  
  query <- glue::glue(
    "INSERT INTO {class_table(which)}
            ({fields})
            VALUES ({values})
            RETURNING *
            ;"
  )
  
  print(query)
  
  dbGetQuery(con, query)
}

# Identify and get attributes ------

# Check that attribute is valid for type of object
attr_valid <- function(which, attr, editable_only = FALSE) {
  if(!attr %in% attributes(which, editable_only)) {
    stop(glue::glue("Valid attributes are {attributes(which, editable_only)}, not {attr}."))
  }
  NULL
}

# Check that calling attribute of particular object is  valid
attr_exists <-function(which , attr, ..., editable_only = FALSE) {
  exists(which, ...)
  attr_valid(which, attr, editable_only)
}

# Get attribute from a particular object; ... should be named ids
get_attr <- function(which, attr, ...) {
  id_str <- make_id_str(...)
  query <- glue::glue(
    "SELECT {attr}
      FROM {class_table(which)}
      WHERE {id_str};"
  )
  
  dbGetQuery(con, query)
}

# Update attribute for particular object; ... should be named ids
set_attr <- function(which, attr, value, ...) {
  attr_exists(which, attr, editable_only = TRUE, ...)
  value <- safe_chr(value)
  
  query <- glue::glue(
    "UPDATE {class_table(which)}
      SET {attr} = {value}
      WHERE {make_id_str(...)}
    RETURNING *;"
  )
  
  dbGetQuery(con, query)
}


# gettable/editable attributes for each object type
attributes <- function(which, editable_only = FALSE) {
  edit_attrs <- list(
    classroom = c("name", "description", "password", "status"), 
    instance = c("identifier", "url", "username", "password"), 
    student = c("name", "email", "consent", "cookie", "other"), 
    event = c("event", "session", "cookie", "other")
  )
  
  static_attrs <- list(
    classroom = c("id"), 
    instance = c("id", "classroomid"), 
    student = c("id", "classroomid"), 
    event = c("id", "classroomid", "instanceid", "studentid")
  )
  
  attrs <- do.call(switch, c(EXPR = which, edit_attrs))
  
  if (!editable_only) attrs <- c(attrs, 
                                 do.call(switch, c(EXPR = which, static_attrs)))
  
  attrs
}

# Specific helpers -----

is_claimed <- function(instance_id) {
  instance_id %in% get("claim")$instanceid
}

archive_class <- function(class_id) {
  query <- glue::glue(
    "UPDATE {class_table('classroom')}
      SET 
        name = concat('ARCHIVED {Sys.Date()}', name),
        password = concat('ARCHIVED {Sys.Date()}', password), 
        description = concat('ARCHIVED {Sys.Date()}', description)
      WHERE classroomid = {class_id};"
  )
  
  dbGetQuery(con, query)
}

