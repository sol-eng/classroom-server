# source("classroom_server_api/helper_funcs.R")
source("helper_funcs.R")

# Do some config/connect to DB
cfg <- config::get(file = "config.yml")
con <<- do.call(pool::dbPool, cfg)
prefix <<- "v1_"
schema <- "classroom"
dbExecute(con, glue::glue("SET search_path={schema};"))

claim <- tbl(con, class_table("claim"))
class <- tbl(con, class_table("classroom"))
student <- tbl(con, class_table("student"))
instance <- tbl(con, class_table("instance"))

#* @apiTitle Classroom Server API

# Classrooms ------

#* Get list of classrooms
#* @get /class
#* @param include_archive include archived classes? Defaults to FALSE
function(include_archive = FALSE) {
  df <- get("classroom") 
  
  include_archive <- as.logical(include_archive)
  
  if (!include_archive) df <- df %>%
      dplyr::filter(!status %in% "ARCHIVE")
  
  df
}

#* Create a classroom
#* @param name name of classroom
#* @param password classroom password
#* @param description classroom description, defaults to NULL
#* @param class_guid guid of classroom (from main API)
#* @post /class
function(res, name, password, class_guid, description = '') {
  classrooms <- get("classroom")
  
  if (name %in% classrooms$name || 
      password %in% classrooms$password ||
      class_guid %in% classrooms$class_guid) {
    msg <- "Classroom names, guids, and passwords must be unique. Yours is not."
    res$status <- 400
    return(list(error = jsonlite::unbox(msg)))
  }
  
  create_object("classroom", 
                name = name, 
                password = password, 
                description = description,
                class_guid = class_guid)
}

#* Get a classroom attribute
#* @param class_id classroomid from classroom table
#* @param attr attribute
#* @get /class_attr
function(res, class_id, attr) {
  class_id <- as.numeric(class_id)
  print(class_id)
  print(attr)
  err <- check_err(res, 
                   attr_exists("classroom", attr, editable_only = TRUE, 
                               classroomid = class_id))
  if (!is.null(err)) return(err)
  
  get_attr("classroom", attr, classroomid = class_id)
}

#* Update classroom attributes
#* @param class_id classroomid from classroom table
#* @param attr attribute
#* @param value new value
#* @post /class_attr
function(res, class_id, attr, value) {
  # Check inputs valid
  class_id <- as.numeric(class_id)
  err <- tryCatch({
    attr_exists("classroom", attr, classroomid = class_id, editable_only = TRUE)
    # Statuses have special requirements
    if (attr == "status"){
      curr_status <- get_attr("classroom", "status",classroomid = class_id)
      if (!value %in%  c("ACTIVE", "ARCHIVE")) {
        stop("Only ACTIVE and ARCHIVE are valid statuses.")
      } else if (curr_status == "ARCHIVE" && value == "ACTIVE") {
        stop("Cannot reactivate ARCHIVEd classrooms.")
      }
    }
  }, error = function(e) {
    res$status <- 400
    handle_err(e)
  })
  if (length(err) > 0) return(err)
  # suffix name, password, and description
  if (attr == "status" && value == "ARCHIVE") archive_class(class_id)
  
  set_attr("classroom", attr, value, classroomid = class_id)
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

#* Create a student
#* @param class_id classroomid from class table
#* @param name full student name
#* @param email student email
#* @param other other details, defaults to NULL
#* @post /student
function(req, res, class_id, name, email, other = '') {
  class_id <- as.numeric(class_id)
  
  err <- check_err(res, exists("classroom", classroomid = class_id))
  if (!is.null(err)) return(err)
  
  create_object("student", 
                classroomid = class_id, 
                name = name, 
                email = email, 
                other = other)
}

#* Get a student attribute
#* @param class_id classroom id
#* @param student_id student id
#* @param attr attribute
#* @get /student_attr
function(res, class_id, student_id, attr) {
  class_id <- as.numeric(class_id)
  student_id <- as.numeric(student_id)
  
  err <- check_err(res, 
                   attr_exists("student", attr, 
                               studentid = student_id, 
                               classroomid = class_id))
  if (!is.null(err)) return(err)
  
  get_attr("student", attr, 
           studentid = student_id, classroomid = class_id)
}

#* Update a student attribute
#* @param class_id classroom id
#* @param student_id student id
#* @param attribute attribute, one of name, email, consent, cookie, other
#* @param value value to update
#* @post /student_attr
function(res, class_id, student_id, attr, value) {
  class_id <- as.numeric(class_id)
  student_id <- as.numeric(student_id)
  
  err <- check_err(res, 
                   attr_exists("student", 
                               attr, 
                               studentid = student_id, 
                               classroomid = class_id))
  if (!is.null(err)) return(err)
  
  set_attr("student", attr, value, 
           classroomid = class_id, studentid = student_id)
}

# Instances ------

#* Get Instances
#* @param class_id id of classroom
#* @get /instance
function(class_id = NULL) {
  df <- get("instance")
  
  if (!is.null(class_id)) df <- dplyr::filter(df, classroomid == class_id)
  
  df
}

#* Add instance
#* @param class_id classroom id
#* @param identifier instance identifier
#* @param url instance URL
#* @param username instance username
#* @param password instance password
#* @post /instance
function(res, class_id, identifier, url, username, password) {
  class_id <- as.numeric(class_id)
  
  err <- check_err(res, 
                   attr_exists("classroom", "id", classroomid = class_id),
                   check_url(url))
  if (!is.null(err)) return(err)
  
  create_object("instance", 
                classroomid = class_id,
                identifier = identifier, 
                url = url, 
                username = username, 
                password = password)
}

#* Add instances
#* @param class_id vector of classroom id
#* @param identifier vector of instance identifiers
#* @param url vector of instance URLs
#* @param username vector of instance usernames
#* @param password vector of instance passwords
#* @post /instances
function(res, class_id, identifier, url, username, password) {
  class_id <- as.numeric(class_id)
  message(glue::glue("Patching instances for {class_id}"))
  err <- tryCatch({
    new_instances <- tibble::tibble(class_id = class_id, 
                                    identifier = identifier, 
                                    url = url, 
                                    username = username, 
                                    password = password)
    attr_exists("classroom", "id", classroomid = class_id[1])
    lapply(url, check_url)
  }, error = function(e) {
    res$status <- 400
    message(e)
    handle_err(e)
  })
  
  message(glue::glue("Got {nrow(new_instances)} new instances"))
  
  # Filter out existing instances
  old_instances <- get('instance') %>% as_tibble()
  new_instances <- new_instances %>%
    dplyr::anti_join(old_instances %>% 
                       select(class_id = classroomid, identifier)) 
  
  message(glue::glue("Got {nrow(new_instances)} (updated) new instances"))
  purrr::pmap(new_instances, function(...){
    args <- list(...)
    print(args)
    create_object("instance",
                  classroomid = args$class_id,
                  identifier = args$identifier,
                  url = args$url,
                  username = args$username,
                  password = args$password)
  })
}

#* Get Instance attributes
#* @param class_id classroom id
#* @param instance_id instance id
#* @param attr attribute
#* @get /instance_attr
function(res, class_id, instance_id, attr) {
  err <- check_err(res, 
                   attr_exists("instance", attr, 
                               classroomid = class_id, 
                               instanceid = instance_id))
  if (!is.null(err)) return(err)
  
  get_attr("instance", attr, 
           classroomid = class_id, instanceid = instance_id)
}

#* Update instance attributes
#* @param class_id classroom id
#* @param instance_id instance id
#* @param attr attribute
#* @param value value
#* @post /instance_attr
function(res, class_id, instance_id, attr, value) {
  class_id <- as.numeric(class_id)
  student_id <- as.numeric(instance_id)
  
  err <- tryCatch({
    attr_exists("instance", 
                attr, 
                instanceid = instance_id, 
                classroomid = class_id)
    if(attr == "url") check_url(value)
  }, error = function(e) {
    res$status <- 400
    handle_err(e)
  })
  if (!is.null(err)) return(err)
  
  
  set_attr("instance", attr, value, 
           classroomid = class_id, instanceid = instance_id)
}


#* Allow student to claim an instance
#* @param class_id classroom
#* @param student_id student
#* @param instance_id instance, defaults to NULL (find an instance)
#* @post /claim_instance
function(res, class_id, student_id, instance_id = NULL) {
  class_id <- as.numeric(class_id)
  student_id <- as.numeric(student_id)
  if(!is.null(instance_id)) instance_id <- as.numeric(instance_id)
  
  err <- tryCatch({
    # check if instance claimed, if supplied
    if(!is.null(instance_id) && is_claimed(instance_id)) {
      stop("That instance is claimed")
    }
    exists("classroom", classroomid = class_id)
    exists("student", classroomid = class_id, studentid = student_id)
    # Check if class, student, and instance exist
    if (!is.null(instance_id)) exists("instance", 
                                      classroomid = class_id, 
                                      instanceid = instance_id)
  }, error = function(e){
    res$status <- 400
    return(handle_err(e))
  })
  if (!is.null(err)) return(err)
  
  # If instance not specified
  if (is.null(instance_id)) {
    tbl(con, class_table('instance')) %>%
      anti_join(tbl(con, class_table('claim')), 
                by = c("instanceid")) %>%
      head(1) %>% 
      pull(instanceid) %>%
      create_object("claim", 
                    instanceid = ., 
                    classroomid = class_id, 
                    studentid = student_id)
  } else {
    # Instance specified
    create_object("claim", 
                  classroomid = class_id, 
                  instanceid = instance_id, 
                  studentid = student_id)
  }
}

#* Get students and instance details for a classroom
#* @param class_id class id
#* @get /student_instances
function(class_id = NULL) {
  tbl(con, class_table('claim')) %>%
    select(classroomid,
           studentid,
           instanceid) %>%
    dplyr::filter(classroomid == class_id) %>%
    left_join(
      select(
        tbl(con, class_table('classroom')),
        classroomid,
        class_description = description,
        class_name = name)) %>%
    left_join(
      select(tbl(con, class_table('instance')),
             classroomid,
             instanceid,
             instance_identifier = identifier,
             instance_url = url,
             instance_user = username,
             instance_pass = password)) %>%
    left_join(
      select(tbl(con, class_table('student')),
             classroomid,
             studentid,
             student_name = name,
             student_email = email)) %>%
    select(-ends_with("id")) %>%
    collect()
}

# Events -----

#* Get Events
#* @get /event
function() {
  get("event")
}

#* Add event
#* @param class_id classroom id
#* @param instance_id instance id
#* @param student_id student id
#* @param event event
#* @param session session
#* @param cookie cookie
#* @param other other
#* @post /event
function(res, event, session, 
         class_id = NULL, instance_id = NULL, student_id = NULL,
         cookie = NULL, other = NULL) {
  
  tryCatch({
    if((!is.null(class_id) && !exists("classroom", classroomid = class_id)) ||
       (!is.null(instance_id) && !exists("instance", instanceid = instance_id)) ||
       (!is.null(student_id) && !exists("student", studentid = student_id))) {
      stop("IDs do not exist.")
    }
  }, 
  error = function(e) {
    res$status <- 400
    return(handle_err(e))
  })
  
  args <- list(
    which = "event", 
    event = event, 
    session = session
  )
  
  if(!is.null(class_id)) args$classroomid <- class_id
  if(!is.null(student_id)) args$studentid <- student_id
  if(!is.null(instance_id)) args$instanceid <- instance_id
  if(!is.null(cookie)) args$cookie <- cookie
  if(!is.null(other)) args$other <- other
  
  do.call(create_object, args)
}

#* Get Event attributes
#* @param event_id event id
#* @param attr attribute
#* @get /event_attr
function(res, event_id, attr) {
  err <- check_err(res, 
                   attr_exists("event", attr, eventid = event_id))
  if (!is.null(err)) return(err)
  
  get_attr("event", attr, eventid = event_id)
}
