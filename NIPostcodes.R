# path - "C:\\Users\\admin\\Documents\\AJAY-CA-2A"

#reading the csv file
NIPostcodes = read.csv("NIPostcodes.csv",sep = ",")

#total number of rows
dim(NIPostcodes)
nrow(NIPostcodes)
#checking the structure of data
str(NIPostcodes)
#showing top 10 rows of data
head(NIPostcodes,n=10)

#Adding suitable title to each attribute
namesofcolumns <- c("Organization Name", "Sub-building Name", "Building Name", "Number", "Primary Thorfar", "Alt Thorfare", "Secondary Thorfare",
                    "Locality", "Townland", "Town", "County", "Postcode", "x-coordinates", "y-coordinates", "Primary Key")
colnames(NIPostcodes) <- namesofcolumns
head(NIPostcodes,n=2)

#moving the primary key to start of the dataset
NIPostcodes <- subset(NIPostcodes, select = c(15,1:14))
View(NIPostcodes)
#missing data
new_NIPostcodes <- na.omit(NIPostcodes)
new_NIPostcodes

NIPostcodes[NIPostcodes == ""] <- NA
View(NIPostcodes)

#Replacing the missing values 
na.sum <- sapply(NIPostcodes, function(y) sum(length(which(is.na(y)))))
na.sum

#mean of missing values
na.mean <- sapply(NIPostcodes, function(y) mean(is.na(y)))
na.mean



#Modifing the County attribute to be a categorising factor.
NIPostcodes$County <- as.factor(NIPostcodes$County)


#creating new dataset with name limavady

Limavady <- subset(NIPostcodes, NIPostcodes$Town =='LIMAVADY', select = c(9:11))
#writing it into external csv file
write.csv(Limavady, file = "Limavady.csv", row.names = FALSE )
str(Limavady)

#acleanNIPOSTCODE data
write.csv(NIPostcodes, file = "CleanNIPostcodeData.csv", row.names = FALSE )


















