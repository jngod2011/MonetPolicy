####################################################
##                    Thesis                      ##
####################################################

# rm(list=ls())
library("sqldf")
library("XLConnect")

setwd("E:/20170421_MA_Data/reuters")

###
# Connect to database
###

db <- dbConnect(drv="SQLite", dbname="reuters.db")
# db <- dbConnect(SQLite(), dbname="reuters.db")

# get a list of all tables
alltables = dbListTables(db)
# get the Articles as a data.frame
dbGetQuery( db,'select * from Articles limit 1' )$content # read 1st article





###
# General Queries
###

# connect to the sqlite file
con = dbConnect(drv="SQLite", dbname="country.sqlite")
p1 = dbGetQuery( con,'select * from populationtable' )
# count the Articles in the SQLite table
p2 = dbGetQuery( con,'select count(*) from areastable' )
# find entries of the DB from the last week
p3 = dbGetQuery(con, "SELECT population WHERE DATE(timeStamp) < DATE('now', 'weekday 0', '-7 days')")
# Clear the results of the last query
dbClearResult(p3)
# Select population with managerial type of job
p4 = dbGetQuery(con, "select * from populationtable where jobdescription like '%manager%'")

###
# End
###
