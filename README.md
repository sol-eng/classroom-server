# Classroom Server

This Shiny application is built to help with workshops. In particular, it takes charge over an oft painful process:

 - uniquely identify users
 - allocate resources (usually a cloud-based VM per student) to those users
 
The [dynamic classroom](./dynamic-classroom) is the most advanced version of the application, is multi-tenant (many workshops at once), and is backed by a Postgres database.

## Getting Started

To test the app locally, you will need a postgres database. Define one in the `config.yml` file at `./dynamic-classroom/config.yml`. 

Then connect to the database and execute the following:

```
source("./dynamic-classroom/create_table.R")
create_all(con, schema = "schema", prefix = "some_prefix_")
```

This creates the tables necessary to run the application. Specify the same schema and prefix in [the app](./dynamic-classroom/app.R), and you're off to the races!