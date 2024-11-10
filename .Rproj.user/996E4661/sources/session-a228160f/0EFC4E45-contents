library(DBI)
library(dplyr)
library(RSQLite)


sqlitePath <- here::here('dbsqlite_test')

## read data and enter save it into local database
pit_cap_data <- fread(here('pit_capacity.csv'))

not_all_na <- function(x) any(!is.na(x))
###. remove columns if there is all na 
pit_cap_data <- pit_cap_data %>% select_if(not_all_na) %>% mutate(Plant_Name = as.character(Plant_Name))

con <- DBI::dbConnect(SQLite(),sqlitePath)
# check if there are tables
dbListTables(con)
# add tables 
dbWriteTable(conn = con, name = 'pit_capacity_data',pit_cap_data)
## check tables again
dbListTables(con)
## now read data
read_data = dbReadTable(con,'pit_capacity_data')
dbDisconnect(con)
