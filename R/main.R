#' @import bnutil
#' @import rms
#' @import dplyr
#' @import data.table
#' @export


#' @export
shinyServerRun = function(input, output, session, context) {

  output$body = renderUI({
    tabsetPanel(
      tabPanel("Input",
        checkboxInput("swapcensor", "Swap censoring"),
        tableOutput("censor"),
        plotOutput("survplot"),
        actionButton("start", "Press to start ...")
      ),
      tabPanel("Result",
        tableOutput("result")
      )
    )
  })

  getDataReactive = context$getData()
  observe({
    getData=getDataReactive$value
    if (is.null(getData)) return()

    bndata = getData()
    df = bndata$data
    df$x = df[[bndata$xAxisColumnName]]

    output$censor = renderTable({
      if(!bndata$hasColors) stop("Need a single data color for the censoring variable")
      if(length(bndata$colorColumnNames) > 1) stop("Need a single data color for the censoring variable")
      df = addCensor()
      pep = subset(df, rowSeq == 1)
      N = as.vector(summary(pep$censor))
      censorTable = data.frame(Censor = levels(pep$censor),
                        Event = c(FALSE, TRUE),
                        N = N)

      return(censorTable)
    })

    output$survplot = renderPlot({
      df = addCensor()
      fit = npsurv(df$survObj ~ 1)
      survplot(fit)
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
      nid = showNotification("Done!", duration = NULL)
      aResult = subset(aResult, select = c("rowSeq", "colSeq", "p", "plr"))
      meta = data.frame(labelDescription = c("rowSeq", "colSeq", "p", "plr"),
                        groupingType = c("rowSeq", "colSeq", "QuantitationType", "QuantitationType"))
      result = AnnotatedData$new(data = aResult, metadata = meta)
      context$setResult(result)
    })
  })
}

