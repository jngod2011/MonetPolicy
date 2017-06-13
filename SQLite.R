####################################################
##                    Thesis                      ##
####################################################

# rm(list=ls())
library("sqldf")
library("XLConnect")

setwd("E:/20170421_MA_Data/reuters")

# Connect to database -----------------------------------------------------

#db <- dbConnect(drv="SQLite", dbname="reuters.db")
db <- dbConnect(SQLite(), dbname="reuters.db")

# get a list of all tables
alltables = dbListTables(db)

# *: id, article_id, content, pub_datetime, date_scraped, title, authors, raw_data, 
# date stuff: https://sqlite.org/lang_datefunc.html

# get the Articles as a data.frame
#dbGetQuery(db,'SELECT * FROM Articles LIMIT 1' )$content # read 1st article
#dbGetQuery(db, "SELECT * FROM Articles WHERE pub_datetime = '2015-01-03 23:51:00' ")
#dbGetQuery(db, "SELECT content, title FROM Articles WHERE pub_datetime = BETWEEN date(
#             '2015-01-03') AND date('2015-01-04') # doesn't work yet

#test1 <- dbGetQuery(db, "SELECT content, title FROM Articles WHERE 
#                    pub_datetime < datetime('2015-01-03 23:51:00','-1 day')")

# alternative for later: determine ids with elastic and reference accordingly

# 2008 and before no data \dots

# restrict to a few articles to make analysis feasible
OMO_20151217 <- dbGetQuery(db, "SELECT content, title, pub_datetime FROM Articles WHERE 
                    pub_datetime BETWEEN datetime('2015-12-17 14:00:00','-5 day') 
                    AND datetime('2015-12-17 14:00:00','+5 day')")
setwd("C:/Users/Admin/Google Drive/Masterthesis")
save(OMO_20151217,file="OMO_20151217.RData")

# Manipulate for TA -------------------------------------------------------

setwd("C:/Users/Admin/Google Drive/Masterthesis")
load("OMO_20151217.RData")
min(OMO_20151217[,3]) # first article
max(OMO_20151217[,3]) # last article 



# General Queries ---------------------------------------------------------

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


# End ---------------------------------------------------------------------


