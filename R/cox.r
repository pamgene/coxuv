#' @export
docox = function(df){
  models = df %>% group_by(rowSeq) %>% do({
    coxreg = coxph(survObj ~ value, data = .)
    data.table(ID = .$ID[1], model = list(coxreg))
  })
  aResult = models %>% group_by(rowSeq) %>% do({
    cm = .$model[[1]]
    out = as.data.frame(summary(cm)$conf.int)
    out = data.frame(out,ID = .$ID, p = summary(cm)$sctest[['pvalue']], plr = summary(cm)$logtest[['pvalue']])
  }) %>% arrange(p)
  return(aResult)
}
#' @export
globalcox = function(df){
  X = acast(df, colSeq ~ rowSeq, value.var = "value")
  time = as.numeric(acast(df, colSeq ~ rowSeq, value.var = "x")[,1])
  event = as.numeric(acast(df, colSeq ~ rowSeq, value.var = "event")[,1])
  sObj = Surv(time = time, event = event)
  aGlobaltest = gt(sObj, X)
}
