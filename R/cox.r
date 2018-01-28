#' @export
docox = function(df){
  models = df %>% group_by(rowSeq, colSeq) %>% do({
    coxreg = coxph(survObj ~ value, data = .)
    data.table(ID = .$ID[1], model = list(coxreg))
  })
  aResult = models %>% group_by(rowSeq, colSeq) %>% do({
    cm = .$model[[1]]
    out = as.data.frame(summary(cm)$conf.int)
    out = data.frame(out,ID = .$ID, p = summary(cm)$sctest[['pvalue']], plr = summary(cm)$logtest[['pvalue']])
  }) %>% arrange(p)
  return(aResult)
}
