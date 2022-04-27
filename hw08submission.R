library(RSQLite)
setwd ("C:/Users/Kelly/Downloads/2021Sem2/STAT405/STAT405Project")

#create connection

dcon <- dbConnect(SQLite(), dbname = "STAT405_crimeCOVIDfinal.sqlite")
#don't forget to download the database.sqlite file instead.

dbSendQuery(conn = dcon, "
PRAGMA foreign_keys = ON;
")

## List tables
dbListTables(dcon)

#load all of covid data
query <- paste0("
SELECT *	
  FROM covid")
res <- dbSendQuery(dcon, query)
covid <- dbFetch(res, -1)
dbClearResult(res)

#load all of crime data
query <- paste0("
SELECT *	
  FROM crimedata")
res <- dbSendQuery(dcon, query)
crime <- dbFetch(res, -1)
dbClearResult(res)


#aggregate crime by premise and type (Gail)
query <- paste0("
 SELECT c.Premise, c.NIBRSDescription, COUNT(c.Incident) AS Incident_totals
    FROM crimedata c
    GROUP BY c.Premise, c.NIBRSDescription")
res <- dbSendQuery(dcon, query)
crimetr <- dbFetch(res, -1)
dbClearResult(res)
View(crimetr)

# total offenses per day (Kelly)
query <- paste0("
 SELECT c.OccurrenceDate, COUNT(c.OffenseCount) AS Total_Offenses
 FROM crimedata c
 GROUP BY c.OccurrenceDate")
res <- dbSendQuery(dcon, query)
crimeperday <- dbFetch(res, -1)
dbClearResult(res)
View(crimeperday)

# number of occurrences by NIBRSDescription (Elijah)
query <- paste0("
 SELECT NIBRSDescription, COUNT(*)
 FROM crimedata 
 GROUP BY NIBRSDescription;")
res <- dbSendQuery(dcon, query)
test <- dbFetch(res, -1)
dbClearResult(res)
View(test)

# Top 10 Dates with the highest occurrences of crimes (Jennifer)

query <- paste0("
 SELECT c.OccurrenceDate, COUNT(c.OffenseCount) AS Total_Offenses
 FROM crimedata c
 GROUP BY c.OccurrenceDate
 ORDER BY Total_Offenses DESC
 LIMIT 10;")
res <- dbSendQuery(dcon, query)
TOP10 <- dbFetch(res, -1)
dbClearResult(res)
View(TOP10)
dbDisconnect(dcon)

# Hourly total offenses  (Naghmeh)
query <- paste0("
 SELECT OccurrenceHour, NIBRSDescription, COUNT(OffenseCount) AS Total_Offenses
 FROM crimedata c
 GROUP BY NIBRSDescription,OccurrenceHour
                ORDER BY OccurrenceHour;")

res <- dbSendQuery(dcon, query)
crime_Hour <- dbFetch(res, -1)
dbClearResult(res)
View(crime_Hour)
