# install dplyr db
install.packages("dbplyr")
# install connector 
install.packages("RMariaDB")
install.packages("RMySQL")

# activate packages
library(DBI)
library(dbplyr)
library(dplyr)
library(RMariaDB)
library(RMySQL)

# connect
mydb = dbConnect(MySQL(), 
                 user='employee_user', 
                 password='Madarijus', 
                 host='healthdata.usm.my', 
                 dbname = "employees",
                 port = 3306)
# test connection
library(dplyr)
con2 <- DBI::dbConnect(RSQLite::SQLite(), path = ":memory:")

# test 2
con3 <- DBI::dbConnect(RMySQL::MySQL(), 
                      host = "database.rstudio.com",
                      user = "hadley",
                      password = rstudioapi::askForPassword("Database password")
)

# test 3
mydb = dbConnect(MySQL(), 
                 user='employee_user', 
                 password='Madarijus', 
                 host='healthdata.usm.my', 
                 dbname = "employees",
                 port = 3306)

mydb
summary(mydb)
dbGetInfo(mydb)
db_list_tables(mydb)

# test 4


library(DBI)
con2 <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "localhost\\SQLEXPRESS", 
                 Database = "datawarehouse", 
                 Trusted_Connection = "True")


dbDisconnect(mydb)


# ref : https://www.slideshare.net/RsquaredIn/rmysql-tutorial-for-beginners 


# test 5

library(odbc)
odbcListDrivers()
sort(unique(odbcListDrivers()[[1]]))

library(DBI)
con4 <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 5.3 ANSI Driver",
                      Server   = "healthdata.usm.my",
                      Database = "employees",
                      UID      = "employee_user",
                      PWD      = "Madarijus",
                      Port     = 3306)
dbDisconnect(con4)






