# Store the links to the zipped json files containing profile data
file_list <- c( "https://s3.amazonaws.com/michaelfy_linkedinquire/a/1339943811.zip",
                "https://s3.amazonaws.com/michaelfy_linkedinquire/a/1340908651.zip",
                "https://s3.amazonaws.com/michaelfy_linkedinquire/a/1341183840.zip")


# download the online LinkedIn data and unzip them into Json files
path <- "~/Desktop/LinkdinData"  #creating a general path to create folder linkedin
path1 <- "~/Desktop/LinkdinData/JsonFile"
# download the online LinkedIn data and unzip them into Json files
for (i in 1:length(file_list)){
  file_name_1 <- substr(file_list[i], nchar(file_list[i])-15, nchar(file_list[i])-15)
  file_name_2 <- substr(file_list[i], nchar(file_list[i])-13, nchar(file_list[i]))
  file_name <- paste(path, file_name_1, "_", file_name_2, sep="")
  download.file(file_list[i], destfile=file_name, method="curl")
  unzip(file_name, exdir=path1)
}


library(jsonlite)
# extracting files that match the regualr expression
alphabet <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "o", "u", "v", "w", "x", "y", "z")
for (j in 1:length(alphabet)){
  regexp <- paste("^", alphabet[j], ".*json$", sep="")
  file_list <- list.files(path= path1, pattern=regexp)
  print(length(file_list))
  
  # keep the data only have full information on location, positions, industry, skills and educations
  linkedin_data <- NULL #giving null to data frame
  for (i in 1:length(file_list)){
    tryCatch({
      jsonData <- fromJSON(paste("~/Desktop/LinkdinData/JsonFile/", file_list[i], sep=""))
      field_name <- names(jsonData)
      # Selecting fileds that do not have Null vaulues
      json_loc <- (jsonData$location != "NULL")
      json_pos <- (jsonData$positions != "NULL")
      json_ind<- (jsonData$industry != "NULL")
      json_sk <- (jsonData$skills != "NULL")
      json_edu <- (jsonData$educations != "NULL")
      # Logically combining them to eliminate risk of missing rows and mismatch
      json <- (json_loc & json_pos & json_ind & json_sk & json_edu) 
      jsonData <- jsonData[json, c("location", "positions", "industry", "skills", "educations")] 
      linkedin_data <- rbind(linkedin_data, jsonData)
    },
    error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
# Stop the downloading after 5 mins of it starting to limit the amount of data to be worked with.


#Storing the Linkedin Data in Mongodb

library(mongolite)
mon_linkedin <- mongo("profiles")
mon_linkedin$drop()
mon_linkedin$insert(linkedin_data)


# getting Industry data from MongoDB
industry <- mon_linkedin$find(fields = '{"industry" :1, "_id":0}')
library(dplyr)
industrydata <- as.data.frame(table(industry))
colnames(industrydata) <- c("Industry", "Frequency") 
industrydata <- arrange(industrydata, desc(Frequency))

# pick the top industries
num <- 20
topindustry <- head(industrydata, 20)


# Bar plot showing Top Industries
ggplot(topindustry, aes(x= Industry, y=Frequency))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  geom_bar(stat = "identity",position="stack", width = 0.5, fill = I("orange"))+
  labs(main = paste("Top", num ,"Industries"), x = "", y = "value")


# Skills for Topmost Industry in demand 

skills <- mon_linkedin$find(query  = '{"industry": "Information Technology and Services"}', fields = '{"skills":1, "_id" : 0}')

IT_skills <- skills %>%
  separate_rows(skills)%>%
  filter(skills != "[[:punct:]]" & skills!= "Management" & skills != "")
IT_skills <- as.data.frame(table(IT_skills$skills)) 
colnames(IT_skills) <- c("Skills", "freq")  
topskills <- IT_skills %>% arrange(desc(freq)) %>%
  head(20)

# Bar plot showing Top skills
ggplot(topskills, aes(x=Skills, y=freq))+
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  geom_bar(stat = "identity",position="stack", width = 0.5, fill = I("orange"))+
  labs(title ="Top 20 Skills", x = "", y = "value")

# Locations 
library(stringr)
library(data.table)
loc <- mon_linkedin$find(fields = '{"industry":1, "location":1, "_id":0}')
location <- loc %>%
  filter(str_detect(location,"New York"))
#merged the top industries and the data in New york  
location <- merge(topindustry, location, by.x = "Industry", by.y = "industry") 
location <- location[,-2]

# Bar plot showing demand Top industries in New York 
ggplot(location, mapping = aes( x= Industry, y = location)) +
  geom_point(aes(color = factor(Industry))) +
  theme(axis.text.x = element_blank())+
  labs(main = "Top Industries in New York by Locations", xlab = "Industry")


# Scraped data

library(tidyverse)
scraped <- as.data.frame(read_csv("project.csv"))
# Tidying up the data
colnames(scraped) <- c("Position", "Company", "Location")
scraped$Location <- gsub("(.*),.*", "\\1", scraped$Location)
scraped <- scraped %>%
  separate(Location, into = c("City", "State"), sep = ",", na.rm = TRUE)


# Storing the data in Mongolite
mon_scraped <- mongo("scraped")
mon_scraped$drop()
mon_scraped$insert(scraped)

# Jobs of a data analyst role
analyst <- mon_scraped$find(query = '{"Position": { "$regex" : "^Data Analyst", "$options": "i"}}', fields = '{"_id":0, "State": 0}')

# Jobs at Buffalo
Buffalo <-  mon_scraped$find(query = '{"City": { "$regex" : "^Buffalo", "$options": "i"}}', fields = '{"_id":0, "State": 0}')

# Jobs at Horizon media
company <- mon_scraped$find(query = '{"Company": "Horizon Media"}', fields = '{"_id":0, "State": 0}')



