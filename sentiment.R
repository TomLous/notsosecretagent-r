library(rmongodb)
library(RTextTools)
library(e1071)
library(tm)
source('utils.R')

mongo <- mongo.create("178.62.232.68")


if(mongo.is.connected(mongo) == TRUE) {
  db <- "AIVD"
  coll <- "AIVD.classified"
  #mongo.get.database.collections(mongo, db)
  
  #mongo.count(mongo, coll)
  
  #mongo.find.all(mongo, coll)
  tweets <- mongo.find.all(mongo, coll,  query='{"_type":"twitter", "language": {"$exists": true}}',fields='{"interaction.content": 1, "language.tag":1, "classification.sentiment":1, "_id":0}', data.frame=TRUE)
  tweets$langcode <- as.factor(tweets$tag)
  
  languageCounts <- sort(table(tweets$langcode), decreasing = TRUE)[0:10]
  languageCounts <- languageCounts[languageCounts > 10]
  filteredLanguages <- languages[languages$language_code %in% names(languageCounts),]
  
  
  
  #tweets$X_type <- as.factor(tweets$X_type)
  tweets$sentiment <- as.factor(tweets$sentiment)
  mtweets <- merge(tweets, filteredLanguages, by.x=c("tag"), by.y=c("language_code"))
  mtweets$tag <- NULL
  
  
}