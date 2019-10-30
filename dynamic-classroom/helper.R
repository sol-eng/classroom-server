    # helpful functions ------------------------
    
    protect_empty <- function(input, out_obj="null"){
        if (input %in% c("''", "", '""') || is.na(input) || 
            is.null(input) || length(input) == 0) {
            return(out_obj)
        } else {
            return(input)
        }
    }
    add_classroom <- function(con, schema, prefix, name, password, status, description = NULL, class_guid = uuid::UUIDgenerate()) {
        description <- description %>% glue::single_quote() %>% protect_empty()
        dbGetQuery(con, glue::glue(
            "INSERT INTO {schema}.{prefix}classroom
            (name, password, status, class_guid, description)
            values ('{name}', '{password}','{status}', '{class_guid}', {description})
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
    set_student_cookie <- function(con, schema, prefix, studentid, cookie) {
        cookie <- cookie %>% glue::single_quote() %>% protect_empty()
        dbGetQuery(con, glue::glue(
            "UPDATE {schema}.{prefix}student
            SET cookie = {cookie}
            WHERE studentid = {studentid}
            RETURNING *
            ;
            "
        ))
    }
    set_student_consent <- function(con, schema, prefix, studentid, consent = NULL) {
        consent <- consent %>% protect_empty("false")
        dbGetQuery(con, glue::glue(
            "UPDATE {schema}.{prefix}student
            SET consent = {consent}
            WHERE studentid = {studentid}
            RETURNING *
            ;
            "
        ))
    }
    set_student_cookie <- function(con, schema, prefix, studentid, cookie = NULL) {
      cookie <- cookie %>% glue::single_quote() %>% protect_empty()
      dbGetQuery(con, glue::glue(
        "UPDATE {schema}.{prefix}student
        SET cookie = {cookie}
        WHERE studentid = {studentid}
        RETURNING *
        ;"
      ))
    }
    add_student <- function(con, schema, 
                            prefix, classroomid, email, name = NULL, 
                            other = NULL,
                            dryrun = FALSE) {
        name <- name %>% glue::single_quote() %>% protect_empty()
        email <- email %>% glue::single_quote()
        other <- other %>% glue::single_quote() %>% protect_empty()
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}student
            (classroomid, name, email, other)
            VALUES ({classroomid}, {name}, {email}, {other})
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
    
    add_instance <- function(con, schema,
                             prefix, classroomid, identifier, url,
                             username, password,
                             dryrun = FALSE) {
        identifier <- identifier %>% glue::single_quote()
        url <- url %>% glue::single_quote()
        username <- username %>% glue::single_quote()
        password <- password %>% glue::single_quote()
        
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}instance
            (classroomid, identifier, url, username, password)
            VALUES ({classroomid}, {identifier}, {url}, {username}, {password})
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
    add_claim <- function(con, schema, prefix,
                          classroomid, studentid, instanceid = NULL
                          , dryrun = FALSE) {
      # do the claim in a single transaction to protect against
      # potential issues with concurrency
      
      if (is.null(instanceid)) {
        # get instanceid from available instances
        query <- glue::glue(
          "WITH new_instance AS (
            SELECT instanceid
            FROM {schema}.{prefix}instance
            WHERE instanceid NOT IN (
              SELECT instanceid
              FROM {schema}.{prefix}claim
              WHERE classroomid = {classroomid}
            )
            AND classroomid = {classroomid}
            LIMIT 1
          )
          INSERT INTO {schema}.{prefix}claim
          (classroomid, instanceid, studentid)
          SELECT {classroomid}, instanceid, {studentid}
          FROM new_instance
          RETURNING *
          ;"
        )
      } else {
        # take the provided instanceid
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}claim
            (classroomid, instanceid, studentid)
            VALUES ({classroomid},{instanceid},{studentid})
            RETURNING *
            ;"
            )
      }
        if (!dryrun) {
            res <- dbGetQuery(con, query)
            return(res)
        } else {
            return(query)
        }
    }
    log_event <- function(con, schema, prefix, event, session,
                          classroomid="", instanceid="",
                          studentid="", other="", cookie="", dryrun=FALSE) {
        event <- glue::single_quote(event) %>% protect_empty()
        other <- glue::single_quote(other) %>% protect_empty()
        cookie <- cookie %>% glue::single_quote() %>% protect_empty()
        session <- session %>% glue::single_quote() %>% protect_empty()
        
        classroomid <- classroomid %>% protect_empty()
        instanceid <- instanceid %>% protect_empty()
        studentid <- studentid %>% protect_empty()
        
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}event
            (event, session, classroomid, instanceid, studentid, other, cookie)
            VALUES ({event}, {session}, {classroomid}, {instanceid}
            , {studentid}, {other}, {cookie})
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
  safe_logical <- function(input, default = FALSE){
    input <- as.logical(input)
    if (is.na(input) || length(input) == 0){
      return(default)
    } else {
      return(input)
    }
  }
  safe_name <- function(name){
    if (is.null(name) || nchar(name) == 0) {
      return(NULL)
    } else if (is.name(name)){
      return(name)
    } else {
      return(as.name(name))
    }
  }
    
is_admin <- function(user) {
  (!is.null(user) && 
    (
      stringr::str_detect(user, "^.*@rstudio.com$") || 
        user == "cole" ||
        user %in% (
          Sys.getenv("CLASS_ADMIN_USER") %>%
            strsplit(split = "\\|") %>%
            .[[1]]
        )
    )) ||
    nchar(Sys.getenv("RSTUDIO_USER_IDENTITY")) > 0
}
    
#reprex::reprex({
#explore <- function(a, con) {
#  a <- glue::single_quote(a)
#  query <- glue::glue("SELECT {a};")
#  print(query)
#  
#  proc <- callr::r_bg(
#    func = function(query, con){print(query); DBI::dbGetQuery(con, query)}
#    , args = list(query = query, con = con)
#    , supervise = TRUE
#    )
#  return(proc)
#}
#
#con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
#
#pr <- explore("test", con)
#pr$read_all_output()
#})
