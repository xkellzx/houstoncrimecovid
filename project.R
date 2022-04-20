
#Clear global environment
rm(list = ls())

library("readxl")

getwd()
setwd ("C:\\Users\\Kelly\\Downloads\\2021Sem2\\STAT405\\STAT405Project\\Data")

crime21 <- read_excel("NIBRSPublicViewDec21.xlsx")
crime20 <- read_excel("NIBRSPublicView.Jan1-Dec31-2020.xlsx")
crime19 <- read_excel("2019_NIBRSPublicView.Jan1-Dec31.xlsx")

jan18_pre <- read_excel("jan18.xls")
feb18_pre <- read_excel("feb18.xls")
mar18_pre <- read_excel("mar18.xls")
apr18_pre <- read_excel("apr18.xls")
may18_pre <- read_excel("may18.xls")
jun18_pre <- read_excel("06-2018.NIBRS_Public_Data_Group_A&B.xlsx")
jul18_pre <- read_excel("07-2018.NIBRS_Public_Data_Group_A&B.xlsx")
aug18_pre <- read_excel("08-2018.NIBRS_Public_Data_Group_A&B.xlsx")
sep18_pre <- read_excel("09-2018.NIBRS_Public_Data_Group_A&B.xlsx")
oct18_pre <- read_excel("10-2018.NIBRS_Public_Data_Group_A&B.xlsx")
nov18_pre <- read_excel("11-2018.NIBRS_Public_Data_Group_A&B.xlsx")
dec18_pre <- read_excel("12-2018.NIBRS_Public_Data_Group_A&B.xlsx")

jan17_pre <- read_excel("jan17.xls")
feb17_pre <- read_excel("feb17.xls")
mar17_pre <- read_excel("mar17.xls")
apr17_pre <- read_excel("apr17.xls")
may17_pre <- read_excel("may17.xls")
jun17_pre <- read_excel("jun17.xls")
jul17_pre <- read_excel("jul17.xls")
aug17_pre <- read_excel("aug17.xls")
sep17_pre <- read_excel("sep17.xls")
oct17_pre <- read_excel("oct17.xlsx")
nov17_pre <- read_excel("nov17.xls")
dec17_pre <- read_excel("dec17.xls")

#only apply once per month after loading in months!
clean_data18 <- function(month){ 
  month <- month[-(1:9),-c(2, 4, 6, 11, 13, 14, 16)]
  names(month) <- as.character(month[1, ])
  month <- month[-1, ]
  month <- as.data.frame(month) #MUST BE TURNED INTO A DATA FRAME
  return(month)
}

#from NHH's work
jun18 <- clean_data18(jun18_pre)
jul18 <- clean_data18(jul18_pre)
aug18 <- clean_data18(aug18_pre)
oct18 <- clean_data18(oct18_pre)
nov18 <- clean_data18(nov18_pre)
dec18 <- clean_data18(dec18_pre)

#NOTE: sep18 is different!
sep18 <- sep18_pre[ -c(1:9),-c(2, 4, 6, 11, 13, 14, 16)]
names(sep18) <- as.character(sep18[1, ])
sep18 <- sep18[-1, ]
sep18 <- as.data.frame(sep18) 

#Reorder and rename the columns(jan-may)

Col_Order_name <- function(month){
  month <- month [ ,c(1,2,3,10,4,5,6,7,8,9)]
  colnames(month) <- c("Occurrence Date","Occurrence Hour","NIBRS Description",
                         "Offense Count","Beat","Premise","Block Range",
                         "Street Name","Street Type","Suffix")
  
  month <- as.data.frame(month) #MUST BE TURNED INTO A DATA FRAME
  
  return(month)
}


# This should be run only once!
jan18 <- Col_Order_name(jan18_pre) 
feb18 <- Col_Order_name(feb18_pre)
mar18 <- Col_Order_name(mar18_pre)
apr18 <- Col_Order_name(apr18_pre)
may18 <- Col_Order_name(may18_pre)

jan17 <- Col_Order_name(jan17_pre) 
feb17 <- Col_Order_name(feb17_pre)
mar17 <- Col_Order_name(mar17_pre)
apr17 <- Col_Order_name(apr17_pre)
may17 <- Col_Order_name(may17_pre)
jun17 <- Col_Order_name(jun17_pre)
jul17 <- Col_Order_name(jul17_pre)
aug17 <- Col_Order_name(aug17_pre)
sep17 <- Col_Order_name(sep17_pre)
oct17 <- Col_Order_name(oct17_pre)
nov17 <- Col_Order_name(nov17_pre)
dec17 <- Col_Order_name(dec17_pre)

# Months Jan-May (crime18_1) have different date format. 

crime18_1 <- rbind(jan18,feb18,mar18,apr18,may18)
crime18_2 <- rbind(jun18,jul18,aug18,sep18,oct18,nov18,dec18)

# Change class of  date column in crime18_1
crime18_1$`Occurrence Date` <- as.Date(crime18_1$`Occurrence Date`,format = "%m/%d/%Y")

jan17$`Occurrence Date` <- as.Date(jan17$`Occurrence Date`,format = "%m/%d/%Y")
feb17$`Occurrence Date` <- as.Date(feb17$`Occurrence Date`,format = "%m/%d/%Y")
mar17$`Occurrence Date` <- as.Date(mar17$`Occurrence Date`,format = "%m/%d/%Y")
apr17$`Occurrence Date` <- as.Date(apr17$`Occurrence Date`,format = "%m/%d/%Y")
may17$`Occurrence Date` <- as.Date(may17$`Occurrence Date`,format = "%m/%d/%Y")
jun17$`Occurrence Date` <- as.Date(jun17$`Occurrence Date`,format = "%m/%d/%Y")
jul17$`Occurrence Date` <- as.Date(jul17$`Occurrence Date`,format = "%m/%d/%Y")
aug17$`Occurrence Date` <- as.Date(aug17$`Occurrence Date`,format = "%m/%d/%Y")
sep17$`Occurrence Date` <- as.Date(sep17$`Occurrence Date`,format = "%m/%d/%Y")
oct17$`Occurrence Date` <- as.Date(oct17$`Occurrence Date`,format = "%m/%d/%Y")
nov17$`Occurrence Date` <- as.Date(nov17$`Occurrence Date`,format = "%m/%d/%Y")
dec17$`Occurrence Date` <- as.Date(dec17$`Occurrence Date`,format = "%m/%d/%Y")

#Stack 2018 data 
crime18 <- rbind(crime18_1,crime18_2)

# Sort columns by date in crime18
date_order <- order(crime18$"Occurrence Date",crime18$"Occurrence Hour")
crime18 <-crime18[date_order, ] 

#remove data from 2015-2017 (rows=1-65)
crime18 <- crime18[crime18$"Occurrence Date" >= as.Date("2018-01-01"),]

#for(i in 1:10){print(typeof(jul17[1,i]))}
#switches to "character" from "double" for first variable 

#merge 2017 data
crime17 <- rbind(jan17, feb17, mar17, apr17, may17, jun17, jul17, aug17, sep17, oct17, nov17, dec17)
#Sort columns by date
date_order <- order(crime17$"Occurrence Date",crime17$"Occurrence Hour")
crime17 <-crime17[date_order, ] 
#remove data from before 2017
crime17 <- crime17[crime17$"Occurrence Date" >= as.Date("2017-01-01"),]

#Check total number of rows
dim(crime18_2)[1] + dim(crime19)[1] + dim(crime20)[1] + dim(crime21)[1] + dim(crime17)[1]

#Add missing columns (fill with NAs) to crime17
crime17$Incident <- rep(NA, dim(crime17)[1])
crime17$NIBRSClass <- rep(NA, dim(crime17)[1])
crime17$StreetNo <- rep(NA, dim(crime17)[1])
crime17$City <- rep(NA, dim(crime17)[1])
crime17$ZipCode <- rep(NA, dim(crime17)[1])


crime18$Incident <- rep(NA, dim(crime18)[1])
crime18$NIBRSClass <- rep(NA, dim(crime18)[1])
crime18$StreetNo <- rep(NA, dim(crime18)[1])
crime18$City <- rep(NA, dim(crime18)[1])
crime18$ZipCode <- rep(NA, dim(crime18)[1])

crime19$StreetNo <- rep(NA, dim(crime19)[1]) 
crime20$StreetNo <- rep(NA, dim(crime20)[1]) 
crime21$BlockRange <- rep(NA, dim(crime21)[1]) 

#Reorder and rename to be consistent with new
Col_Order_Old <- function(month){
  month <- month[ ,c(11, 1, 2, 12, 3, 4, 5, 6, 13, 7, 8, 9, 10, 14, 15)]
  colnames(month) <- c("Incident", "OccurrenceDate","OccurrenceHour", "NIBRSClass", 
                       "NIBRSDescription", "OffenseCount","Beat","Premise",
                       "StreetNo", "BlockRange","StreetName","StreetType","Suffix",
                       "City", "ZIPCode")
  return(month)
}

crime17 <- Col_Order_Old(crime17)
crime18 <- Col_Order_Old(crime18)

#Reorder and rename to be consistent with old
Col_Order_1920 <- function(month){
  month <- month[ ,c(1, 2, 3, 4, 5, 6, 7, 8, 15, 9, 10, 11, 12, 13, 14)]
  colnames(month) <- c("Incident", "OccurrenceDate","OccurrenceHour", "NIBRSClass",
                       "NIBRSDescription", "OffenseCount","Beat","Premise",
                       "StreetNo", "BlockRange","StreetName","StreetType","Suffix",
                       "City", "ZIPCode")
  return(month)
}

crime19 <- Col_Order_1920(crime19)
crime20 <- Col_Order_1920(crime20)

crime21 <- crime21[ ,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 15, 10, 11, 12, 13, 14)]
colnames(crime21) <- c("Incident", "OccurrenceDate","OccurrenceHour", "NIBRSClass",
                     "NIBRSDescription", "OffenseCount","Beat","Premise",
                     "StreetNo", "BlockRange","StreetName","StreetType","Suffix",
                     "City", "ZIPCode")

crime <- rbind(crime17, crime18, crime19, crime20, crime21)

dim(crime)
View(crime)

save(crime, file="crime.Rda") 

crime <- as.data.frame(crime)

# Gail's crime data as 
setwd ("C:\\Users\\Kelly\\Downloads\\2021Sem2\\STAT405\\STAT405Project\\Data")
load(file = "crime.Rda")

# Standardizing NIBRSDescription for All Years
year17 <- crime[format(crime$OccurrenceDate, format="%Y") == "2017",]
year18 <- crime[format(crime$OccurrenceDate, format="%Y") == "2018",]
year19 <- crime[format(crime$OccurrenceDate, format="%Y") == "2019",]
year20 <- crime[format(crime$OccurrenceDate, format="%Y") == "2020",]
year21 <- crime[format(crime$OccurrenceDate, format="%Y") == "2021",]

# 2017
for (row in seq(dim(year17)[1])){
  if (year17$NIBRSDescription[row] == as.character(1)){
    year17$NIBRSDescription[row] <- "All Other Offenses"
  } else if (year17$NIBRSDescription[row] == "Theft" | year17$NIBRSDescription[row] == "Auto Theft" | year17$NIBRSDescription[row] == "AutoTheft" | year17$NIBRSDescription[row] == "Robbery"){
    year17$NIBRSDescription[row] <- "Theft, Robbery"
  } else if (year17$NIBRSDescription[row] == "Murder"){
    year17$NIBRSDescription[row] <- "Murder, Manslaughter"
  } else if (year17$NIBRSDescription[row] == "Aggravated Assault" | year17$NIBRSDescription[row] == "Rape"){
    year17$NIBRSDescription[row] <- "Assault, Rape"
  } else if (year17$NIBRSDescription[row] == "Burglary"){
    year17$NIBRSDescription[row] <- "Burglary, Breaking, Entering"
  } else {
    year18$NIBRSDescription[row] <- "All Other Offenses"
  }
}

View(year17)

# 2018
for (row in seq(dim(year18)[1])){
  if (year18$NIBRSDescription[row] == "Intimidation"){
    year18$NIBRSDescription[row] <- "Intimidation"
  } else if (year18$NIBRSDescription[row] == "AutoTheft" | year18$NIBRSDescription[row] == "Motor vehicle theft" | year18$NIBRSDescription[row] == "Theft from motor vehicle" | year18$NIBRSDescription[row] == "Robbery" | year18$NIBRSDescription[row] == "All other larceny" | year18$NIBRSDescription[row] == "Theft from building" | year18$NIBRSDescription[row] == "Theft from motor vehicle" | year18$NIBRSDescription[row] == "Theft of motor vehicle parts or accessory" | year18$NIBRSDescription[row] == "Pocket-picking" | year18$NIBRSDescription[row] == "Purse-snatching" | year18$NIBRSDescription[row] == "Stolen property offenses"){
    year18$NIBRSDescription[row] <- "Theft, Robbery"
  } else if (year18$NIBRSDescription[row] == "Murder" | year18$NIBRSDescription[row] == "Murder, non-negligent" | year18$NIBRSDescription[row] == "Negligent manslaughter"){
    year18$NIBRSDescription[row] <- "Murder, Manslaughter"
  } else if (year18$NIBRSDescription[row] == "Aggravated Assault" | year18$NIBRSDescription[row] == "Rape" | year18$NIBRSDescription[row] == "Simple assualt" | year18$NIBRSDescription[row] == "Forcible sodomy" | year18$NIBRSDescription[row] == "Forcible fondling" | year18$NIBRSDescription[row] == "Forcible rape" | year18$NIBRSDescription[row] == "Statutory rape" | year18$NIBRSDescription[row] == "Peeping tom" | year18$NIBRSDescription[row] == "Sexual assault with an object" | year18$NIBRSDescription[row] == "Human Trafficking/Commericial Sex Act" | year18$NIBRSDescription[row] == "Human Trafficking/Involuntary Servitude"){
    year18$NIBRSDescription[row] <- "Assault, Rape"
  } else if (year18$NIBRSDescription[row] == "Burglary" | year18$NIBRSDescription[row] == "Burglary, Breaking, and Entering" | year18$NIBRSDescription[row] == "Trespass of real property"){
    year18$NIBRSDescription[row] <- "Burglary, Breaking, Entering"
  } else if (year18$NIBRSDescription[row] == "Destruction, damage, vandalism"){
    year18$NIBRSDescription[row] <- "Destruction, damage, vandalism"
  } else if (year18$NIBRSDescription[row] == "Drug, narcotic violations" | year18$NIBRSDescription[row] == "Driving under the influence" | year18$NIBRSDescription[row] == "Drug equipment violations" | year18$NIBRSDescription[row] == "Drunkenness"){
    year18$NIBRSDescription[row] <- "Drugs, Alcohol Violations"
  } else {
    year18$NIBRSDescription[row] <- "All Other Offenses"
  }
}

View(year18)

# 2019
for (row in seq(dim(year19)[1])){
  if (year19$NIBRSDescription[row] == "Intimidation"){
    year19$NIBRSDescription[row] <- "Intimidation"
  } else if (year19$NIBRSDescription[row] == "AutoTheft" | year19$NIBRSDescription[row] == "Motor vehicle theft" | year19$NIBRSDescription[row] == "Theft from motor vehicle" | year19$NIBRSDescription[row] == "Robbery" | year19$NIBRSDescription[row] == "All other larceny" | year19$NIBRSDescription[row] == "Theft from building" | year19$NIBRSDescription[row] == "Theft from motor vehicle" | year19$NIBRSDescription[row] == "Theft of motor vehicle parts or accessory" | year19$NIBRSDescription[row] == "Pocket-picking" | year19$NIBRSDescription[row] == "Purse-snatching" | year19$NIBRSDescription[row] == "Stolen property offenses"){
    year19$NIBRSDescription[row] <- "Theft, Robbery"
  } else if (year19$NIBRSDescription[row] == "Murder" | year19$NIBRSDescription[row] == "Murder, non-negligent" | year19$NIBRSDescription[row] == "Negligent manslaughter"){
    year19$NIBRSDescription[row] <- "Murder, Manslaughter"
  } else if (year19$NIBRSDescription[row] == "Aggravated Assault" | year19$NIBRSDescription[row] == "Rape" | year19$NIBRSDescription[row] == "Simple assualt" | year19$NIBRSDescription[row] == "Forcible sodomy" | year19$NIBRSDescription[row] == "Forcible fondling" | year19$NIBRSDescription[row] == "Forcible rape" | year19$NIBRSDescription[row] == "Statutory rape" | year19$NIBRSDescription[row] == "Peeping tom" | year19$NIBRSDescription[row] == "Sexual assault with an object" | year19$NIBRSDescription[row] == "Human Trafficking/Commericial Sex Act" | year19$NIBRSDescription[row] == "Human Trafficking/Involuntary Servitude"){
    year19$NIBRSDescription[row] <- "Assault, Rape"
  } else if (year19$NIBRSDescription[row] == "Burglary" | year19$NIBRSDescription[row] == "Burglary, Breaking, and Entering" | year19$NIBRSDescription[row] == "Trespass of real property"){
    year19$NIBRSDescription[row] <- "Burglary, Breaking, Entering"
  } else if (year19$NIBRSDescription[row] == "Destruction, damage, vandalism"){
    year19$NIBRSDescription[row] <- "Destruction, damage, vandalism"
  } else if (year19$NIBRSDescription[row] == "Drug, narcotic violations" | year19$NIBRSDescription[row] == "Driving under the influence" | year19$NIBRSDescription[row] == "Drug equipment violations" | year19$NIBRSDescription[row] == "Drunkenness"){
    year19$NIBRSDescription[row] <- "Drugs, Alcohol Violations"
  } else {
    year19$NIBRSDescription[row] <- "All Other Offenses"
  }
}

View(year19)

# 2020
for (row in seq(dim(year20)[1])){
  if (year20$NIBRSDescription[row] == "Intimidation"){
    year20$NIBRSDescription[row] <- "Intimidation"
  } else if (year20$NIBRSDescription[row] == "AutoTheft" | year20$NIBRSDescription[row] == "Motor vehicle theft" | year20$NIBRSDescription[row] == "Theft from motor vehicle" | year20$NIBRSDescription[row] == "Robbery" | year20$NIBRSDescription[row] == "All other larceny" | year20$NIBRSDescription[row] == "Theft from building" | year20$NIBRSDescription[row] == "Theft from motor vehicle" | year20$NIBRSDescription[row] == "Theft of motor vehicle parts or accessory" | year20$NIBRSDescription[row] == "Pocket-picking" | year20$NIBRSDescription[row] == "Purse-snatching" | year20$NIBRSDescription[row] == "Stolen property offenses"){
    year20$NIBRSDescription[row] <- "Theft, Robbery"
  } else if (year20$NIBRSDescription[row] == "Murder" | year20$NIBRSDescription[row] == "Murder, non-negligent" | year20$NIBRSDescription[row] == "Negligent manslaughter"){
    year20$NIBRSDescription[row] <- "Murder, Manslaughter"
  } else if (year20$NIBRSDescription[row] == "Aggravated Assault" | year20$NIBRSDescription[row] == "Rape" | year20$NIBRSDescription[row] == "Simple assualt" | year20$NIBRSDescription[row] == "Forcible sodomy" | year20$NIBRSDescription[row] == "Forcible fondling" | year20$NIBRSDescription[row] == "Forcible rape" | year20$NIBRSDescription[row] == "Statutory rape" | year20$NIBRSDescription[row] == "Peeping tom" | year20$NIBRSDescription[row] == "Sexual assault with an object" | year20$NIBRSDescription[row] == "Human Trafficking/Commericial Sex Act" | year20$NIBRSDescription[row] == "Human Trafficking/Involuntary Servitude"){
    year20$NIBRSDescription[row] <- "Assault, Rape"
  } else if (year20$NIBRSDescription[row] == "Burglary" | year20$NIBRSDescription[row] == "Burglary, Breaking, and Entering" | year20$NIBRSDescription[row] == "Trespass of real property"){
    year20$NIBRSDescription[row] <- "Burglary, Breaking, Entering"
  } else if (year20$NIBRSDescription[row] == "Destruction, damage, vandalism"){
    year20$NIBRSDescription[row] <- "Destruction, damage, vandalism"
  } else if (year20$NIBRSDescription[row] == "Drug, narcotic violations" | year20$NIBRSDescription[row] == "Driving under the influence" | year20$NIBRSDescription[row] == "Drug equipment violations" | year20$NIBRSDescription[row] == "Drunkenness"){
    year20$NIBRSDescription[row] <- "Drugs, Alcohol Violations"
  } else {
    year20$NIBRSDescription[row] <- "All Other Offenses"
  }
}

View(year20)

# 2021
for (row in seq(dim(year21)[1])){
  if (year21$NIBRSDescription[row] == "Intimidation"){
    year21$NIBRSDescription[row] <- "Intimidation"
  } else if (year21$NIBRSDescription[row] == "AutoTheft" | year21$NIBRSDescription[row] == "Motor vehicle theft" | year21$NIBRSDescription[row] == "Theft from motor vehicle" | year21$NIBRSDescription[row] == "Robbery" | year21$NIBRSDescription[row] == "All other larceny" | year21$NIBRSDescription[row] == "Theft from building" | year21$NIBRSDescription[row] == "Theft from motor vehicle" | year21$NIBRSDescription[row] == "Theft of motor vehicle parts or accessory" | year21$NIBRSDescription[row] == "Pocket-picking" | year21$NIBRSDescription[row] == "Purse-snatching" | year21$NIBRSDescription[row] == "Stolen property offenses"){
    year21$NIBRSDescription[row] <- "Theft, Robbery"
  } else if (year21$NIBRSDescription[row] == "Murder" | year21$NIBRSDescription[row] == "Murder, non-negligent" | year21$NIBRSDescription[row] == "Negligent manslaughter"){
    year21$NIBRSDescription[row] <- "Murder, Manslaughter"
  } else if (year21$NIBRSDescription[row] == "Aggravated Assault" | year21$NIBRSDescription[row] == "Rape" | year21$NIBRSDescription[row] == "Simple assualt" | year21$NIBRSDescription[row] == "Forcible sodomy" | year21$NIBRSDescription[row] == "Forcible fondling" | year21$NIBRSDescription[row] == "Forcible rape" | year21$NIBRSDescription[row] == "Statutory rape" | year21$NIBRSDescription[row] == "Peeping tom" | year21$NIBRSDescription[row] == "Sexual assault with an object" | year21$NIBRSDescription[row] == "Human Trafficking/Commericial Sex Act" | year21$NIBRSDescription[row] == "Human Trafficking/Involuntary Servitude"){
    year21$NIBRSDescription[row] <- "Assault, Rape"
  } else if (year21$NIBRSDescription[row] == "Burglary" | year21$NIBRSDescription[row] == "Burglary, Breaking, and Entering" | year21$NIBRSDescription[row] == "Trespass of real property"){
    year21$NIBRSDescription[row] <- "Burglary, Breaking, Entering"
  } else if (year21$NIBRSDescription[row] == "Destruction, damage, vandalism"){
    year21$NIBRSDescription[row] <- "Destruction, damage, vandalism"
  } else if (year21$NIBRSDescription[row] == "Drug, narcotic violations" | year21$NIBRSDescription[row] == "Driving under the influence" | year21$NIBRSDescription[row] == "Drug equipment violations" | year21$NIBRSDescription[row] == "Drunkenness"){
    year21$NIBRSDescription[row] <- "Drugs, Alcohol Violations"
  } else {
    year21$NIBRSDescription[row] <- "All Other Offenses"
  }
}

View(year21)

# Combine all clean data
crimedata <- rbind(year17, year18, year19, year20, year21)
View(crimedata)
save(crimedata, file="crimedata.Rda") 


# Check dimensions
dim(crimedata)
dim(crime)

############################### COVID Data ##################################

covid <- read.csv("covidpositiverate.csv")
save(covid, file="covid.Rda") 
View(covid)
dim(covid)
names(covid)

########################### Data Visualizations ###############################

library(dplyr)
library(ggplot2)

setwd ("C:\\Users\\Kelly\\Downloads\\2021Sem2\\STAT405\\STAT405Project\\Data")
load(file = "crimedata.Rda")
save(crimedata, file="crimedata.Rda") 
View(crimedata)
dim(crimedata)

############## Crime Visualizations ###############

#  Number of Crimes for Each Type of Crime
ggplot(data = crimedata) + aes(x = NIBRSDescription) + geom_bar(stat = "count", fill = c("#AED1EF", "#C1D5E2", "#D4D9D5", "#E8DDC7", "#F2DFC1", "#F2D9C8", "#F1CED5", "#F0B9EF")) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9), 
        axis.text.y = element_text(size = 9), 
        axis.title = element_text(size = 11), 
        title = element_text(size = 13)) + 
  xlab("Type of Crime") + ylab("Number of Offenses") + 
  labs(title = "Number of Offenses by Type of Crime") +
  theme(plot.title = element_text(hjust = 0.5)) 

# Pie Chart of Types of Crimes
ggplot(data = crimedata) + aes(x = factor(1), fill = NIBRSDescription) + geom_bar(stat = "count") + 
  theme(title = element_text(size = 17)) + 
  labs(title = "Number of Offenses by Type of Crime") +
  coord_polar(theta = 'y', start = 0) + theme_void() + 
  theme(plot.title = element_text(hjust = 0.5)) 


# Line Chart of Crime Rates For Every Year

# Bar Chart of Crime Rates For Each Year Separated By Each Type of Crime

# Texas / Harris County Map

############ Crime and Covid Data Visualizations ############

#################### Strings ######################

# Free Text Data
library (readr)
urlfile="https://raw.githubusercontent.com/MickeysClubhouse/COVID-19-rumor-dataset/master/Data/news/news.csv"
covidnews <- read_csv(url(urlfile))
save(covidnews, file="covidnews.Rda") 
colnames(covidnews) <- c('1','2','news', '3')
View(covidnews)
names(crimedata)

library(stringr)
# string length
min(str_length(covidnews$news))
max(str_length(covidnews$news))
mean(str_length(covidnews$news))
median(str_length(covidnews$news))

# string replace all



