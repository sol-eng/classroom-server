# Various Helpers for Classroom Management API

# Misc ------
# get table name
class_table <- function(which) {
  stopifnot(which %in% c("claim", "classroom", "event", "instance", "student"))
  
  paste0(prefix, which)
}

# get tables
get <- function(table) {
  dbReadTable(con, class_table(table))
}

# cast empty to null
protect_empty <- function(input, out_obj = "null"){
  if (input %in% c("''", "") || is.na(input) || 
      is.null(input) || length(input) == 0) {
    return(out_obj)
  } else {
    return(input)
  }
}

safe_chr <- function(x) {
  x %>% glue::single_quote() %>% protect_empty()
}

get_classroom_status <- function(class_id) {
  class_id <- as.numeric(class_id)
  
  query <- glue::glue(
    "SELECT 
      status 
    FROM {class_table('classroom')} 
    WHERE classroomid = {class_id};"
  )
  dbGetQuery(con, query)
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