#setting the working directory
setwd("C:\\Users\\admin\\Downloads\\2015-01 (3)\\2015-01")

#merging all csv files of 2015,2016,2017
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){
  read.csv(i, header=FALSE, skip=4)
})
crimedata <- do.call(rbind.data.frame, All)


col_names<-c("Crime ID"," Month","Reported by","Falls within","Longitude","Latitude","Location","LSOA code","LSOA name","Crime type","Last outcome","Context")
colnames(crimedata)<-col_names
dim(crimedata)
#writing csv file of ALLNICRIMEDATA
write.csv(crimedata,file = "ALLNICRIMEDATA.csv")

#reading csv file of ALLNICRIMEDATA
ALLNICRIMEDATA=read.csv("ALLNICRIMEDATA.csv",sep = ",")
#count and structure
dim(ALLNICRIMEDATA)
str(ALLNICRIMEDATA)

ALLNICRIMEDATA=subset(ALLNICRIMEDATA,select = c(2:13))
dim(ALLNICRIMEDATA)
colnames(ALLNICRIMEDATA)
#removing CrimeID, Reported by, Falls within, LSOA code, LSOA name, last outcome and context
ALLNICRIMEDATA=subset(ALLNICRIMEDATA,select = c(2,5:7,10))
#checking the structure of data
dim(ALLNICRIMEDATA)
str(ALLNICRIMEDATA)

#factorising the crimetypr
ALLNICRIMEDATA$Crime.type=as.factor(ALLNICRIMEDATA$Crime.type)
str(ALLNICRIMEDATA)


#modifying location attributes

ALLNICRIMEDATA$Location<- gsub("On or near"," ",ALLNICRIMEDATA$Location,ignore.case = FALSE)

View(ALLNICRIMEDATA)

#showing top rows
head(ALLNICRIMEDATA)

#RANDOM SAMPLING
colSums(is.na(ALLNICRIMEDATA["Location"]))
str(ALLNICRIMEDATA)
dim(ALLNICRIMEDATA)
summary(ALLNICRIMEDATA)

random_crime_sample<-ALLNICRIMEDATA[sample(1:nrow(ALLNICRIMEDATA),1000,replace = F),]
View(random_crime_sample)
#checking dimensions 
dim(random_crime_sample)


library(dplyr)

#reading CLEANNIPOSTCODE
CLEANNIPOSTCODES<-read.csv("C:\\Users\\admin\\Downloads\\CleanNIPostcodeData.csv")


#Convert the location attributes in random crime sample to upper case
random_crime_sample$Location <- toupper(random_crime_sample$Location)
#in NIPOSTCODES data set using Thorfar and postcode in dataframe
new_ds <- CLEANNIPOSTCODES[, c(6, 13)]
new_ds


# deleting the duplicate values in primary thorfare column
new_ds <- new_ds[!duplicated(new_ds$`Primary Thorfare`),]
# column names for new dataset
colnames(new_ds) <- c("Primary Thorfare", "Postcode")
str(new_ds)

# add a new column to the random crime sample dataset and place the values as NA
random_crime_sample$Postcode <-NA
head(random_crime_sample, 5)

# add the values for postcode column by matching the location with primary thorfare in new_ds
random_crime_sample$Postcode <- new_ds$Postcode[match(random_crime_sample$Location, 
                                                      new_ds$`Primary Thorfare`)]
#Structure of random set
str(random_crime_sample)
# number of rows
NROW(random_crime_sample)
View(random_crime_sample)
# we already appended the postcodes to the random sample set above
# Now we will Save the modified random crime sample data frame as random_crime_sample.csv.
write.csv(random_crime_sample, "random_crime_sample.csv")


str(random_crime_sample)

updated_random_sample <- data.frame(random_crime_sample)
colnames(updated_random_sample) <- c("Month", "Longitude", "Latitude", "Location", "Crime.type", "Postcode")
head(updated_random_sample, 3)

chart_data <- updated_random_sample
# Sort chart_data w.r.t Postcode and crime type
chart_data <- chart_data[order(chart_data$Postcode == "BT1", chart_data$Crime.type), ]
chart_data

# create a new chart dataset that contains postcode = "BT1"
new_chart <- filter(chart_data, grepl('BT1', Postcode))
new_chart
new_chart[order(new_chart$Postcode == 'BT1', new_chart$Crime.type), ]
str(new_chart)

# Summary of crime type as per postcode and location

crime_type <- data.frame(new_chart$Crime.type)
library(plyr)
crime_type <- ddply(crime_type, .(new_chart$Crime.type), nrow)
colnames(crime_type) <- c("Crime_type", "Count")
crime_type


CrimeData <- table(chart_data$Crime.type)
barplot(CrimeData, main = "Crime Type Frequency", xlab = "Crime Type", ylab = "Frequency", col = "blue", border = "green",
        
        density = 150)


