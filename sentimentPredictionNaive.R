suppressMessages(library(RTextTools))
suppressMessages(library(e1071))

script.dir <- dirname(sys.frame(1)$ofile)

  
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0) {
  #text <- "glad happy relieved ok, nice great awesome ok, lets go"
  #text <- "Hate isis kill muder death"
  #text <- "Israel violates the ceasefire by bombing in Gaza and in rafah \nkilling at least 3 in Rafah , Breaks 65 UN resolutions till now\n\n#ICC4Israel"
  #text <- "dddd"
  #text <- "ok "
  #stop(0, call.=FALSE)
} else if (length(args)>1) {
  text <- args[1]
  
  if (length(args)==2) {
    path <- args[2]
  }else{
    path <- "./"
  }
}


mat <- create_matrix(text, "english", minDocFreq=1, removeStopwords=FALSE, removeNumbers=TRUE,  # we can also removeSparseTerms
                     stemWords=FALSE, stripWhitespace=TRUE, toLower=TRUE)

matrix <- as.matrix(mat)

model <- readRDS(paste0(path,"sentimentNaiveBayes.rds"))
labels <- readRDS(paste0(path,"sentimentLabels.rds"))


predicted <- predict(model, matrix)


cat(as.character(predicted), sep='')
