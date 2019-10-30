create_schema <- function(schema){
  glue::glue(
    "CREATE SCHEMA IF NOT EXISTS {schema};"
  )
}

create_table_classroom <- function(schema, prefix){
  glue::glue(
    "CREATE TABLE {schema}.{prefix}classroom ( 
    classroomid serial PRIMARY KEY NOT NULL,
    name text NOT NULL UNIQUE,
    description text,
    password text NOT NULL UNIQUE,
    status text NOT NULL DEFAULT 'CREATED',
    claim_per_student integer NOT NULL DEFAULT 1,
    class_guid text NOT NULL UNIQUE
    );")
}

create_table_student <- function(schema, prefix){
  glue::glue(
    "CREATE TABLE {schema}.{prefix}student (
    studentid serial PRIMARY KEY NOT NULL,
    classroomid integer NOT NULL REFERENCES {schema}.{prefix}classroom (classroomid),
    name text,
    email text NOT NULL,
    consent boolean NOT NULL DEFAULT false,
    cookie text,
    other text,
    UNIQUE(classroomid, email)
    );"
  )
}

create_table_instance <- function(schema, prefix){
  glue::glue(
    "CREATE TABLE {schema}.{prefix}instance (
    instanceid serial PRIMARY KEY NOT NULL,
    classroomid integer NOT NULL REFERENCES {schema}.{prefix}classroom (classroomid),
    identifier text NOT NULL UNIQUE,
    url text NOT NULL UNIQUE,
    username text NOT NULL,
    password text NOT NULL
    );"
  )
}

create_table_claim <- function(schema, prefix){
  glue::glue(
    "CREATE TABLE {schema}.{prefix}claim (
      claimid serial PRIMARY KEY NOT NULL,
      classroomid integer NOT NULL REFERENCES {schema}.{prefix}classroom (classroomid),
      instanceid integer NOT NULL REFERENCES {schema}.{prefix}instance (instanceid),
      studentid integer NOT NULL REFERENCES {schema}.{prefix}student (studentid),
      cookie text,
      info text
    );"
  )
}

create_table_event <- function(schema, prefix){
  glue::glue(
    "CREATE TABLE {schema}.{prefix}event (
      eventid serial PRIMARY KEY NOT NULL,
      event text NOT NULL,
      session text NOT NULL,
      classroomid integer REFERENCES {schema}.{prefix}classroom (classroomid),
      instanceid integer REFERENCES {schema}.{prefix}instance (instanceid),
      studentid integer REFERENCES {schema}.{prefix}student (studentid),
      cookie text, 
      other text
    );"
  )
}

add_created_lm_by <- function(schema, prefix, table){
  glue::glue(
    "ALTER TABLE {schema}.{prefix}{table}
    ADD COLUMN created timestamp NOT NULL DEFAULT now(),
    ADD COLUMN createdby text NOT NULL DEFAULT current_user,
    ADD COLUMN lastmodified timestamp NOT NULL DEFAULT now(),
    ADD COLUMN lastmodifiedby text NOT NULL DEFAULT current_user
    ;
    "
  )
}

create_ts_trigger <- function(schema, prefix){
  glue::glue(
    "CREATE FUNCTION {schema}.{prefix}_update_created_lm_by()
      RETURNS trigger
    AS $body$
    BEGIN

    -- SHOULD EXECUTE BEFORE INSERT / UPDATE

    IF TG_OP = 'INSERT' THEN
    	NEW.created := current_timestamp;
    	NEW.createdby := current_user;
    END IF;
    
    NEW.lastmodified := current_timestamp;
    NEW.lastmodifiedby := current_user;
    
    RETURN NEW;
    
    END;
    $body$
    LANGUAGE PLPGSQL
    ;"
  )
}

add_ts_trigger <- function(schema, prefix, table){
  glue::glue(
    "CREATE TRIGGER tr_{prefix}{table}_update_created_lm_user
      BEFORE INSERT OR UPDATE ON {schema}.{prefix}{table}
    FOR EACH ROW
    EXECUTE PROCEDURE {schema}.{prefix}_update_created_lm_by()
    ;"
  )
}

create_archive_trigger <- function(schema, prefix){
  stop("Not yet implemented")
  glue::glue(
    "CREATE FUNCTION {schema}.{prefix}_archive_trigger()
      RETURNS trigger
    AS $body$
    BEGIN
    
    -- SHOULD EXECUTE AFTER INSERT/UPDATE/DELETE

    -- Expects tgop and id columns in front

    IF TG_OP = 'INSERT' OR TG_OP = 'UPDATE' THEN
      NEW.lastmodified := current_timestamp;
      NEW.lastmodifiedby := current_user;

      
    END IF;

    IF TG_OP = 'DELETE' THEN
      OLD.lastmodified := current_timestamp;
      OLD.lastmodifiedby := current_user;

      
       

    END IF;
    
    END;
    $body$
    LANGUAGE PLPGSQL
    ;"
  )
}

add_archive_trigger <- function(schema, prefix, table){
  stop("Not yet implemented")
  glue::glue(
    "CREATE TRIGGER tr_{prefix}archive{table}
      AFTER INSERT OR UPDATE OR DELETE ON {schema}.{prefix}{table}
      FOR EACH ROW
      EXECUTE PROCEDURE {schema}.{prefix}_archive_trigger()
    ;"
  )
}


create_all <- function(con, schema, prefix) {
  dbExecute(con,create_schema(schema));
  
  dbExecute(con,create_table_classroom(schema, prefix));
  dbExecute(con,create_table_student(schema, prefix));
  dbExecute(con,create_table_instance(schema, prefix));
  dbExecute(con,create_table_claim(schema, prefix));
  dbExecute(con,create_table_event(schema, prefix));
  
  dbExecute(con, create_ts_trigger(schema, prefix))
  
  dbExecute(con, add_created_lm_by(schema,prefix, "claim"))
  dbExecute(con, add_created_lm_by(schema,prefix, "classroom"))
  dbExecute(con, add_created_lm_by(schema,prefix, "event"))
  dbExecute(con, add_created_lm_by(schema,prefix, "instance"))
  dbExecute(con, add_created_lm_by(schema,prefix, "student"))
  
  dbExecute(con, add_ts_trigger(schema, prefix, "claim"))
  dbExecute(con, add_ts_trigger(schema, prefix, "classroom"))
  dbExecute(con, add_ts_trigger(schema, prefix, "event"))
  dbExecute(con, add_ts_trigger(schema, prefix, "instance"))
  dbExecute(con, add_ts_trigger(schema, prefix, "student"))

}

# add_classroom(con, "classroom", "test", "Test Classroom 1", "somepass", "ACTIVE")
