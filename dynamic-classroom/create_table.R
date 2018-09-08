create_table_classroom <- function(schema, prefix){
  glue::glue("CREATE TABLE {schema}.{prefix}_classroom (
             classroomid serial PRIMARY KEY NOT NULL,
             name text NOT NULL UNIQUE,
             password text NOT NULL
             );")
}

create_table_student <- function(schema, prefix){
  glue::glue(
    "CREATE TABLE {schema}.{prefix}_student (
    studentid serial PRIMARY KEY NOT NULL,
    classroomid integer NOT NULL FOREIGN KEY REFERENCES {schema}.{prefix}_classroom (classroomid),
    name text,
    email text,
    consent boolean NOT NULL DEFAULT false,
    cookie text
    );"
  )
}

create_table_instance <- function(schema, prefix){
  glue::glue(
    "CREATE TABLE {schema}.{prefix}_instance (
    instanceid serial PRIMARY KEY NOT NULL,
    classroomid integer NOT NULL FOREIGN KEY REFERENCES {schema}.{prefix}_classroom (classroomid),
    identifier text NOT NULL,
    url text NOT NULL,
    user text NOT NULL,
    password text NOT NULL
    );"
  )
}