suppressMessages(library(rmongodb))
suppressMessages(library(RTextTools))
suppressMessages(library(e1071))
#library(magrittr)

script.dir <- dirname(sys.frame(1)$ofile)

source('utils.R')

mongo <- mongo.create("178.62.232.68")


#if(mongo.is.connected(mongo) == TRUE) {
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

  
  tweets$sentiment <- as.factor(tweets$sentiment)
  mtweets <- merge(tweets, filteredLanguages, by.x=c("tag"), by.y=c("language_code"))
  mtweets$tag <- NULL
  mtweets <- droplevels(mtweets)
  
  mtweets_en <- mtweets[mtweets$language=="english",]
  
  mat <- create_matrix(mtweets_en$content, "english", minDocFreq=1, removeStopwords=FALSE, removeNumbers=TRUE,  # we can also removeSparseTerms
                       stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE)
  
  matrix <- as.matrix(mat)
  
  num_tweets = nrow(mtweets_en)
  
  sample_size = floor(0.75 * num_tweets)
  
  mtweets_train <- head(mtweets_en, sample_size)
  matrix_train <- head(matrix, sample_size)
  mtweets_test <- tail(mtweets_en, num_tweets-sample_size)
  matrix_test <- tail(matrix, num_tweets-sample_size)
  
  classifier = naiveBayes(matrix_train, mtweets_train$sentiment)
  predicted <- predict(classifier, matrix_test)
  
  #matrix_test
  
  #predicted
  
  saveRDS(classifier, "sentimentNaiveBayes.rds",compress=F)
  
  
  accuracy <- recall_accuracy(mtweets_test$sentiment, predicted)
  
  #accuracy
  
  
 # num_sentiment <- as.numeric(mtweets_test$sentiment)
  
  #container = create_container(mat, as.numeric(mtweets_en$sentiment), trainSize=1:sample_size, testSize=(sample_size+1):num_tweets,virgin=FALSE)
  
  #models = train_models(container, algorithms=c("MAXENT" , "SVM", "TREE"))
  #model = train_model(container, "SVM")
  
  #results = classify_models(container, models)
  #results = classify_model(container, model)
  
  #table(num_sentiment, results[,"FORESTS_LABEL"])
  #table(num_sentiment, results[,"MAXENTROPY_LABEL"])
  
  # recall accuracy
  #recall_accuracy(num_sentiment, results[,"FORESTS_LABEL"])
  #recall_accuracy(num_sentiment, results[,"MAXENTROPY_LABEL"])
  #recall_accuracy(num_sentiment, results[,"TREE_LABEL"])
  #recall_accuracy(num_sentiment, results[,"BAGGING_LABEL"])
  #recall_accuracy(num_sentiment, results[,"SVM_LABEL"])
  
  #analytics = create_analytics(container, results)
  #summary(analytics)
  #head(analytics@document_summary)
  #analytics@ensemble_summary
  
  #N=4
  #set.seed(2014)
  #cross_validate(container,N,"MAXENT")
  #cross_validate(container,N,"TREE")
  #cross_validate(container,N,"SVM")
  #cross_validate(container,N,"RF")
  
  #saveRDS(models, "sentimentModels.rds",compress=F)
  #saveRDS(model, "sentimentModel.rds",compress=F)
  #saveRDS(mat, "sentimentMatrix.rds",compress=F)
  #saveRDS(levels(mtweets_test$sentiment), "sentimentLabels.rds",compress=F)
  
  
  
  
#}