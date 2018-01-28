library(bnutil)
library(coxuv)
library(rms)
library(dplyr)
library(data.table)


getData = function() {
  bndata = AnnotatedData$new(data = df_pfs, metadata = mdf_pfs)
}

setResult = function(annotatedResult){

}



bnMessageHandler = bnshiny::BNMessageHandler$new()
bnMessageHandler$getDataHandler = getData
bnMessageHandler$setResultHandler = setResult

bnshiny::startBNTestShiny('coxuv', sessionType='run', bnMessageHandler=bnMessageHandler)
