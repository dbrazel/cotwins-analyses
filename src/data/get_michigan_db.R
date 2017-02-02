# Connect to the Michigan MySQL databases, extract selected tables,
# convert to tibbles, and write to RDS files

library(readr)
library(RMySQL)
library(dplyr)

# Get env vars
env_var_names <- c("USER_DATABASE_USER",
             "USER_DATABASE_PW",
             "USER_DATABASE_IP",
             "USER_DATABASE_DBNAME",
             "APP_DATABASE_USER",
             "APP_DATABASE_PW",
             "APP_DATABASE_IP",
             "APP_DATABASE_DBNAME")

env_vars <- Sys.getenv(env_var_names)

# Connect to the databases and set the connections to disconnect, in case
# script is terminated prematurely
user_con <- dbConnect(MySQL(), user = env_vars[[1]], password = env_vars[[2]],
                      host = env_vars[[3]], dbname = env_vars[[4]])
app_con <- dbConnect(MySQL(), user = env_vars[[5]], password = env_vars[[6]],
                     host = env_vars[[7]], dbname = env_vars[[8]])

# Ensure that we will close the DB connections even if an error occurs
# add = TRUE is necessary to have multiple on.exit statements
on.exit(dbDisconnect(user_con))
on.exit(dbDisconnect(app_con), add = TRUE)

# Get all rows and columns from the desired tables and write them to RDS files
# with the date in the file name
user_tables <- c("google_location", 
                 "surveyentries",
                 "sys_account_switch_log",
                 "term_vector",
                 "term_vector_frequency",
                 "token_device",
                 "track_site",
                 "user_domain_name_visits",
                 "user_location",
                 "user_location_google_location",
                 "user_mobile_session",
                 "user_tweet")

today <- format(Sys.Date(), "_%m_%d_%y")

# Get all records in each table and write them out to RDS format
for(tabl in user_tables) {
  query <- dbSendQuery(user_con, paste0("SELECT * FROM ", tabl, ";"))
  df <- tbl_df(dbFetch(query, n = -1))
  write_rds(df, file.path("data", "raw", paste0("Michigan_DB_", tabl, today, ".rds")))
}

# Get only the columns we need from the app DB because there is sensitive data there
query <- dbSendQuery(app_con, "SELECT id, creation_date, co_twin_id, 
                     alternate_id, colorado_id, app_type FROM users;")
df <- tbl_df(dbFetch(query, n = -1))
write_rds(df, file.path("data", "raw", paste0("Michigan_DB_users", today, ".rds")))

# Close the DB connections
dbDisconnect(user_con)
dbDisconnect(app_con)
