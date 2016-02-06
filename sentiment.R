library(rmongodb)
library(RTextTools)
library(e1071)

mongo <- mongo.create("178.62.232.68")

if(mongo.is.connected(mongo) == TRUE) {
  db <- "AIVD"
  coll <- "AIVD.classified"
  #mongo.get.database.collections(mongo, db)
  
  #mongo.count(mongo, coll)
  
  #mongo.find.all(mongo, coll)
  tweets <- mongo.find.all(mongo, coll,  query='{"_type":"twitter", "language": {"$exists": true}}',fields='{"interaction.content": 1, "language.tag":1, "classification.sentiment":1,  "_type":1, "_id":1}', data.frame=TRUE)
}