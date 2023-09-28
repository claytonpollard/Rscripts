library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Variance of Sampled Data"),
  sidebarLayout(
    sidebarPanel(
      div(style = "width:100%;", actionButton("sampleButton", "Get 1 Sample of 50", style = "width:100%; text-align:center;")),
      div(style = "width:100%;", actionButton("sampleButton5", "Get 5 Samples of 50", style = "width:100%; text-align:center;")),
      div(style = "width:100%;", actionButton("sampleButton10", "Get 10 Samples of 50", style = "width:100%; text-align:center;")),
      div(style = "width:100%;", actionButton("sampleButton50", "Get 50 Samples of 50", style = "width:100%; text-align:center;")),
      div(style = "width:100%;", actionButton("sampleButton100", "Get 100 Samples of 50", style = "width:100%; text-align:center;")),
      div(style = "width:100%;", actionButton("sampleButton500", "Get 500 Samples of 50", style = "width:100%; text-align:center;")),
      div(style = "width:100%;", actionButton("sampleButton1000", "Get 1000 Samples of 50", style = "width:100%; text-align:center;")),
      tags$hr(),
      verbatimTextOutput("popVariance"),
      tags$hr(),
      verbatimTextOutput("recentVarianceN"),
      verbatimTextOutput("recentVarianceN1"),
      verbatimTextOutput("recentVarianceN2"),
      tags$hr(),
      verbatimTextOutput("totalSamples"),
      tags$hr(),
      verbatimTextOutput("avgVarianceN"),
      verbatimTextOutput("avgVarianceN1"),
      verbatimTextOutput("avgVarianceN2")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Unrestricted", 
                 plotOutput("plotN"),
                 plotOutput("plotN1"),
                 plotOutput("plotN2")
        ),
        tabPanel("Restricted", 
                 fluidRow(
                   column(3, numericInput("ymin_offset", "Y-Min offset from Population Variance", value = -10)), 
                   column(3, numericInput("ymax_offset", "Y-Max offset from Population Variance", value = 10)) 
                 ),
                 plotOutput("rplotN"),
                 plotOutput("rplotN1"),
                 plotOutput("rplotN2")
        )
      )
    )
  )
)


server <- function(input, output) {
  data <- rnorm(1000000, mean=100, sd=15)
  true_variance <- var(data)
  true_mean <- mean(data)
  variances <- reactiveVal(data.frame(
    sampleNumber = numeric(0),
    N = numeric(0),
    N1 = numeric(0),
    N2 = numeric(0),
    varN = numeric(0),
    varN1 = numeric(0),
    varN2 = numeric(0),
    mean = numeric(0)
  ))
  
  sampleAndCalculate <- function(size) {
    sampleData <- sample(data, size)
    n <- length(sampleData)
    varN <- sum((sampleData - mean(sampleData))^2) / n
    varN1 <- sum((sampleData - mean(sampleData))^2) / (n - 1)
    varN2 <- if(n - 2 > 0) sum((sampleData - mean(sampleData))^2) / (n - 2) else NA
    newVariances <- data.frame(
      sampleNumber = nrow(variances()) + 1,
      N = ifelse(nrow(variances()) == 0, varN, (variances()$N[nrow(variances())] * (nrow(variances())) + varN) / (nrow(variances()) + 1)),
      N1 = ifelse(nrow(variances()) == 0, varN1, (variances()$N1[nrow(variances())] * (nrow(variances())) + varN1) / (nrow(variances()) + 1)),
      N2 = ifelse(nrow(variances()) == 0, varN2, (variances()$N2[nrow(variances())] * (nrow(variances())) + varN2) / (nrow(variances()) + 1)),
      varN = varN,
      varN1 = varN1,
      varN2 = varN2,
      mean = mean(sampleData)
    )
    variances(rbind(variances(), newVariances))
  }
  
  observeEvent(input$sampleButton, {
    sampleAndCalculate(50)
  })
  
  observeEvent(input$sampleButton5, {
    for(i in 1:5) sampleAndCalculate(50)
  })
  
  observeEvent(input$sampleButton10, {
    for(i in 1:10) sampleAndCalculate(50)
  })

  observeEvent(input$sampleButton50, {
    for(i in 1:50) sampleAndCalculate(50)
  })
  
  observeEvent(input$sampleButton100, {
    for(i in 1:100) sampleAndCalculate(50)
  })

  observeEvent(input$sampleButton500, {
    for(i in 1:500) sampleAndCalculate(50)
  })
  
  observeEvent(input$sampleButton1000, {
    for(i in 1:1000) sampleAndCalculate(50)
  })
  
  output$plotN <- renderPlot({
    ggplot(variances(), aes(x = sampleNumber)) + 
      geom_point(aes(y = N)) + 
      geom_point(aes(y = varN), color = "blue", alpha = 0.35) + 
      geom_hline(yintercept = true_variance, color = "red", linetype = "solid") + 
      labs(x = "Sample Number", y = "Variance") + 
      ggtitle("Variance with N")
  })
  
  output$plotN1 <- renderPlot({
    ggplot(variances(), aes(x = sampleNumber)) + 
      geom_point(aes(y = N1)) + 
      geom_point(aes(y = varN1), color = "blue", alpha = 0.35) + 
      geom_hline(yintercept = true_variance, color = "red", linetype = "solid") + 
      labs(x = "Sample Number", y = "Variance") + 
      ggtitle("Variance with N-1")
  })
  
  output$plotN2 <- renderPlot({
    ggplot(variances(), aes(x = sampleNumber)) + 
      geom_point(aes(y = N2)) + 
      geom_point(aes(y = varN2), color = "blue", alpha = 0.35) + 
      geom_hline(yintercept = true_variance, color = "red", linetype = "solid") + 
      labs(x = "Sample Number", y = "Variance") + 
      ggtitle("Variance with N-2")
  })
  
  output$popVariance <- renderText({
    paste("Population Variance: ", round(true_variance, 2))
  })
  
  output$recentVarianceN <- renderText({
    if(nrow(variances()) > 0)
      paste("Recent Sample Variance (N): ", round(tail(variances()$varN, n=1), 2))
    else
      "Recent Sample Variance (N): NA"
  })
  
  output$recentVarianceN1 <- renderText({
    if(nrow(variances()) > 0)
      paste("Recent Sample Variance (N-1): ", round(tail(variances()$varN1, n=1), 2))
    else
      "Recent Sample Variance (N-1): NA"
  })
  
  output$recentVarianceN2 <- renderText({
    if(nrow(variances()) > 0)
      paste("Recent Sample Variance (N-2): ", round(tail(variances()$varN2, n=1), 2))
    else
      "Recent Sample Variance (N-2): NA"
  })

  output$totalSamples <- renderText({
    total_samples <- nrow(variances())
    paste("Number of Times Sampled: ", total_samples)
  })
  
  output$avgVarianceN <- renderText({
    if(nrow(variances()) > 0)
      paste("Average Variance (N): ", round(tail(variances()$N, n=1), 2))
    else
      "Average Variance (N): NA"
  })
  
  output$avgVarianceN1 <- renderText({
    if(nrow(variances()) > 0)
      paste("Average Variance (N-1): ", round(tail(variances()$N1, n=1), 2))
    else
      "Average Variance (N-1): NA"
  })
  
  output$avgVarianceN2 <- renderText({
    if(nrow(variances()) > 0)
      paste("Average Variance (N-2): ", round(tail(variances()$N2, n=1), 2))
    else
      "Average Variance (N-2): NA"
  })

  sd_original <- sd(data) # get standard deviation of the original data
  
  renderRestrictedPlot <- function(plot_data, title, column, ymin, ymax) {
    ggplot(plot_data, aes(x = sampleNumber)) + 
      geom_point(aes_string(y = column)) + 
      geom_point(aes(y = get(column)), color = "blue", alpha = 0.35) + 
      geom_hline(yintercept = true_variance, color = "red", linetype = "solid") + 
      labs(x = "Sample Number", y = "Variance") + 
      ggtitle(title) +
      ylim(ymin, ymax) # setting the restricted y range
  }
  
  output$rplotN <- renderPlot({
    ggplot(variances(), aes(x = sampleNumber)) + 
      geom_point(aes(y = N)) + 
      geom_point(aes(y = varN), color = "blue", alpha = 0.35) + 
      geom_hline(yintercept = true_variance, color = "red", linetype = "solid") + 
      labs(x = "Sample Number", y = "Variance") + 
      ggtitle("Variance with N") +
      coord_cartesian(ylim = c(true_variance + input$ymin_offset, true_variance + input$ymax_offset))
  })
  
  output$rplotN1 <- renderPlot({
    ggplot(variances(), aes(x = sampleNumber)) + 
      geom_point(aes(y = N1)) + 
      geom_point(aes(y = varN1), color = "blue", alpha = 0.35) + 
      geom_hline(yintercept = true_variance, color = "red", linetype = "solid") + 
      labs(x = "Sample Number", y = "Variance") + 
      ggtitle("Variance with N-1") +
      coord_cartesian(ylim = c(true_variance + input$ymin_offset, true_variance + input$ymax_offset))
  })
  
  output$rplotN2 <- renderPlot({
    ggplot(variances(), aes(x = sampleNumber)) + 
      geom_point(aes(y = N2)) + 
      geom_point(aes(y = varN2), color = "blue", alpha = 0.35) + 
      geom_hline(yintercept = true_variance, color = "red", linetype = "solid") + 
      labs(x = "Sample Number", y = "Variance") + 
      ggtitle("Variance with N-2") +
      coord_cartesian(ylim = c(true_variance + input$ymin_offset, true_variance + input$ymax_offset))
  })
  
}

shinyApp(ui, server)