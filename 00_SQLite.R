##########################################################
###                       Thesis                       ###
##########################################################

##########################################################
### --- Author: Manuel von Krosigk
### --- Date: 2017-07-14
### --- Description: connect to SQLite database
##########################################################

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

# End ---------------------------------------------------------------------
