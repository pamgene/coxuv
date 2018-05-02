library(bnutil)
library(coxuv)
library(rms)
library(dplyr)
library(data.table)
library(globaltest)
library(reshape2)


getData = function() {
  bndata = AnnotatedData$new(data = coxuv_df, metadata = coxuv_mdf)
}

setResult = function(annotatedResult){
}

bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getData
bnMessageHandler$setResultHandler = setResult

bnshiny::startBNTestShiny('coxuv', sessionType='run', bnMessageHandler=bnMessageHandler)
