library(shiny)
source("aa_confusion_matrix_bind.R")
source("aa_confusion_matrix_stratified.R")
source("aa_confusion_matrix.R")
source("aa_estimator_stratified_ratio.R")
source("aa_estimator_stratified.R")
source("aa_stratified.R")
source("aa_plot.R")
source("firstup.R")

cit <- "Stehman, S. V., 2014. Estimating area and map accuracy for stratified random sampling when the strata are different from the map classes. Int. J. Remote Sens. 35, 4923–4939."
version <- "(0.1 alpha)"
disclaimer <- 'The Software and code samples available on this website are provided "as is" without warranty of any kind, either express or implied. Use at your own risk.'

ui <- fluidPage(
  
  titlePanel(paste("Stratified estimation of map accuracy", version)),

  p(disclaimer),
  p(cit),
  
 # tags$style(type='text/css', ".selectize-input { font-size: 12px; line-height: 14px;} .selectize-dropdown { font-size: 12px; line-height: 14px; }"),
  
 sidebarLayout(
   sidebarPanel(
     fileInput(inputId='sf', label='Samples:', 
                        multiple = F, accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      selectInput("sf_referenceSamples", "Reference classes", choices = "None"),
      selectInput("sf_mapSamples", "Map classes", choices = "None"),
      selectInput("sf_stratumCode", "Strata", choices = "None"),

      fileInput(inputId='wf', label='Stratum weights:', 
                multiple = F, accept=c("text/csv", "text/comma-separated-values,text/plain",
                                       ".csv")),
      selectInput("wf_codes", "Strata", choices = "None"),
      selectInput("wf_weights", "Stratum weights/area", choices = "None"),
      
      tableOutput(outputId='outWeightTable')
    ),
    mainPanel(
        tableOutput(outputId='outConfusionMatrixRaw'),
        textOutput(outputId='txtOverallAccuracy'),
        p(),
        tableOutput(outputId='outConfusionMatrix'),
        tableOutput(outputId='outAreaEstimates'),
        plotOutput(outputId='outAccuracyPlot'),
        downloadButton("downloadMatrixAdjusted", "Export confusion matrix"),
        downloadButton("downloadStats", "Export statistics")
     )
  )
)




server <- function(input, output, session) {
  
  observe({
    updateSelectInput(session, "sf_referenceSamples", choices = c('None', sampleHeader()))
  })
  
  observe({
    updateSelectInput(session, "sf_mapSamples", choices = c('None', sampleHeader())) # [sampleHeader() != input$sf_referenceSamples]
  })
  
  observe({
    updateSelectInput(session, "sf_stratumCode", choices = c('None', sampleHeader())) # [sampleHeader() != input$sf_referenceSamples]
  })

  observe({
    updateSelectInput(session, "wf_codes", choices = c('None', weightHeader())) #[weightHeader() != input$stratumCode]) 
  })
  
  observe({
    updateSelectInput(session, "wf_weights", choices = c('None', weightHeader())) #[weightHeader() != input$stratumCode]) 
  })
  
  sampleData <- reactive({
    req(input$sf)
    df <- read.csv(input$sf$datapath, stringsAsFactors=F)
    return(df)
  })
  
  sampleHeader <- reactive({names(sampleData())})
  
  weightData <- reactive({
    req(input$wf)
    df <- read.csv(input$wf$datapath, stringsAsFactors=F)
    return(df)
  })
  
  weightHeader <- reactive({return(names(weightData()))})
  
  stratumCodes <- reactive({
    req(input$sf_stratumCode != 'None')
    #req(input$wf_codes != 'None')  # %in% weightHeader()
    if (input$wf_codes == 'None') {
      return(as.data.frame.table(table(stratumSamples()), stringsAsFactors = F)[,1])
    } else {
      return(weightData()[, input$wf_codes])
    }
  })
  
  stratumN <- reactive({
    req(input$sf_stratumCode != 'None') # , input$stratumCode %in% weightHeader()
    if (input$wf_weights == 'None') {
      return(as.data.frame.table(table(stratumSamples()), stringsAsFactors = F)[,2])
    } else {
      return(weightData()[, input$wf_weights])
    }
  })
  
  stratumSamples <- reactive({
    req(input$sf_stratumCode != 'None') # , input$stratumCode %in% weightHeader()
    return(sampleData()[, input$sf_stratumCode])
  })
  
  mapSamples <- reactive({
    req(input$sf_mapSamples != 'None')
    return(sampleData()[, input$sf_mapSamples])
  })
  
  referenceSamples <- reactive({
    req(input$sf_referenceSamples != 'None')
    return(sampleData()[, input$sf_referenceSamples])
  })
  
  aaResults <- reactive({
    #req(input$sf_stratumCode != 'None', input$wf_codes != 'None')
    aa <- aa_stratified(stratumSamples(), referenceSamples(), mapSamples(), h=stratumCodes(), N_h=stratumN())
    return(aa)
  })
  
  aaStats <- reactive({
    stats <- merge(aaResults()$stats, aaResults()$area)
    names(stats) <- c("Class", "User's", "SE", "Producer's", "SE", "Area proportion", "SE")
    return(stats)
  })
  
  output$outWeightTable <- renderTable(
    rownames = F, bordered=T, striped=T,
    {
      return(data.frame(Stratum=stratumCodes(), Weight=stratumN()))
    })

  output$outConfusionMatrixRaw <- renderTable(
    rownames = T, bordered=T, striped=T, digits=0, spacing='xs', caption='Confusion matrix',
    {
         return(aa_confusion_matrix(referenceSamples(), mapSamples()))
    })

  output$outConfusionMatrix <- renderTable(
    rownames = T, bordered=T, striped=T, na='', spacing='xs', caption='Adjusted confusion matrix',
    {
      if (!is.null(aaResults())) return(aa_confusion_matrix_bind(aaResults(), proportion = T))
    })
  
  output$outAreaEstimates <- renderTable(
    bordered=T, striped=T, na='', spacing='xs', caption='Adjusted accuracy and area stats',
    {
      if (!is.null(aaStats())) return(aaStats())
    })
  
  output$txtOverallAccuracy <- renderText({
    oa <- formatC(aaResults()$accuracy[1], format='f', digits=3)
    se <- formatC(aaResults()$accuracy[2], format='f', digits=3)
    paste0("Overall accuracy (SE): ", oa, ' (', se,')')
  })
  
  output$outAccuracyPlot <- renderPlot( {
    aa_plot(aaResults()$stats)
  })
  
  output$downloadMatrixAdjusted <- downloadHandler(
    
    filename = function(){ 
      paste0(substr(input$sf$name, 1, nchar(input$sf$name)-4), "_accuracy.csv")}
    , content= function(file) {
      write.csv(aa_confusion_matrix_bind(aaResults(), proportion = T), file)
    }
  )

  output$downloadStats <- downloadHandler(
    
    filename = function(){ 
      paste0(substr(input$sf$name, 1, nchar(input$sf$name)-4), "_stats.csv")}
    , content= function(file) {
      write.csv(aaStats(), file, row.names=F)
    }
  )

  
  # output$confusionMatrix <- renderTable(
  #   rownames = T, bordered=T, striped=T,
  #   {
  #     return(as.data.frame.matrix(table(sampleData()[,1], sampleData()[,2])))
  #   })

   # output$outAreaEstimates <- renderTable(
   #   rownames = T, bordered=T, striped=T,
   #   {
   #     return(aaResults()$area)
   #   })
  
}

shinyApp(ui=ui, server=server)