suppressMessages(library(RTextTools))
suppressMessages(library(e1071))

  
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  text <- "glad happy relieved ok, nice great awesome ok, lets go"
  text <- "Hate isis kill muder death"
  text <- "Israel violates the ceasefire by bombing in Gaza and in rafah \nkilling at least 3 in Rafah , Breaks 65 UN resolutions till now\n\n#ICC4Israel"
  #text <- "dddd"
  #text <- "ok "
  #stop(0, call.=FALSE)
} else if (length(args)==1) {
  text <- args[1]
}



predictionData <- list(text)

#models <- readRDS("sentimentModels.rds")
model <- readRDS("sentimentModel.rds")
mat <- readRDS("sentimentMatrix.rds")
labels <- readRDS("sentimentLabels.rds")


predMatrix <- create_matrix(predictionData, "english", minDocFreq=1, removeStopwords=FALSE, removeNumbers=TRUE,  # we can also removeSparseTerms
                            stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE, originalMatrix=mat)

predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)
results <- classify_model(predictionContainer, model)


results$REAL_LABEL <- labels[as.numeric(as.character(results$SVM_LABEL))]

cat(results[1,2],';',results[1,3], sep='')




