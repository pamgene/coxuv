#' @import bnutil
#' @import rms
#' @import dplyr
#' @import data.table
#' @import survival
#' @import globaltest
#' @import reshape2
#' @export
shinyServerRun = function(input, output, session, context) {

  output$body = renderUI({
    fluidPage(
        checkboxInput("swapcensor", "Swap censoring"),
        tableOutput("censor"),
        plotOutput("survplot"),
        helpText("Test for global association of mesaurements with survival:"),
        tableOutput("globocox"),
        actionButton("start", "Press to start ...")
    )
  })

  getDataReactive = context$getData()
  observe({
    getData=getDataReactive$value
    if (is.null(getData)) return()

    bndata = getData()
    df = bndata$data
    df$x = as.numeric(df[[bndata$xAxisColumnName]])

    output$censor = renderTable({
      if(any(is.na(df$x))) stop("Non-numeric or NA data detected for Survival Time")
      if(!bndata$hasColors) stop("Need a single data color for the censoring variable")
      if(length(bndata$colorColumnNames) > 1) stop("Need a single data color for the censoring variable")
      df = addCensor()
      pep = subset(df, rowSeq == min(rowSeq))
      N = as.vector(summary(pep$censor))
      censorTable = data.frame(Censor = levels(pep$censor),
                        Event = c(FALSE, TRUE),
                        N = N)

      return(censorTable)
    })

    output$survplot = renderPlot({
      df = addCensor()
      pep = subset(df, rowSeq == min(rowSeq))
      fit = npsurv(pep$survObj ~ 1)
      survplot(fit)
    })

    output$globocox = renderTable({
      df = addCensor()
      gtest = globalcox(df)
      result = show(gtest)
    })

    addCensor = reactive({
      df$censor = as.factor(df[[bndata$colorColumnNames]])
      if (length(levels(df$censor)) != 2){
        stop("The data color / censoring variable must have exactly two levels")
      }
      if(input$swapcensor){
        df$censor = relevel(df$censor, levels(df$censor)[2])
      }
      df$event = df$censor == levels(df$censor)[2]
      df$survObj = Surv(time = df$x, event = df$event)
      return(df)
    })

    observeEvent(input$start,ignoreInit = TRUE,
    {
      df = addCensor()
      aResult = docox(df)
      aResult = data.frame(aResult, colSeq = 1)
      nid = showNotification("Done!", duration = NULL)
      aResult = subset(aResult, select = c("rowSeq", "colSeq", "p", "plr"))
      meta = data.frame(labelDescription = c("rowSeq", "colSeq", "p", "plr"),
                        groupingType = c("rowSeq", "colSeq", "QuantitationType", "QuantitationType"))
      result = AnnotatedData$new(data = aResult, metadata = meta)
      context$setResult(result)
    })
  })
}

