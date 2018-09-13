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
      cookie <- cookie %>% single_quote() %>% protect_empty()
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
                          classroomid, instanceid, studentid
                          , dryrun = FALSE) {
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}claim
            (classroomid, instanceid, studentid)
            VALUES ({classroomid},{instanceid},{studentid})
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
    log_event <- function(con, schema, prefix, event,
                          classroomid="", instanceid="",
                          studentid="", other="", cookie="", dryrun=FALSE) {
        event <- glue::single_quote(event) %>% protect_empty()
        other <- glue::single_quote(other) %>% protect_empty()
        cookie <- cookie %>% glue::single_quote() %>% protect_empty()
        classroomid <- classroomid %>% protect_empty()
        instanceid <- instanceid %>% protect_empty()
        studentid <- studentid %>% protect_empty()
        other <- other %>% protect_empty()
        
        query <- glue::glue(
            "INSERT INTO {schema}.{prefix}event
            (event, classroomid, instanceid, studentid, other, cookie)
            VALUES ({event}, {classroomid}, {instanceid}
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
    