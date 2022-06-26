library(shiny)
library(ggplot2)
library(tidyr)

server <- function(input, output, session) {
  data <- read.csv(file="./data.csv")
  data_unlabelled <- data[3:4]
  data_scaled <- scale(data_unlabelled)
  data_scaled_names <- cbind(data[1:4],data_scaled)
  names(data_scaled_names)[5] <- 'task1_zscore'
  names(data_scaled_names)[6] <- 'task2_zscore'
  
  observe ({
    updateSelectInput(session,
                      "selectedClass",
                      choices = data$class)
  })
  
  observe ({
    updateSelectInput(session,
                      "selectedStudent",
                      choices = (subset(data, class == input$selectedClass))$name)
  })

  individualData <- reactive({
    req(input$selectedStudent)
    subset(data_scaled_names, name == input$selectedStudent)[3:6]
#    cbind(((pivot_longer((subset(data_scaled_names, name == input$selectedStudent)[1:2]), cols=c(task1,task2),values_to="mark"))[2]),((pivot_longer((subset(data_scaled_names, name == input$selectedStudent)[3:4]),cols=c(task1_zscore,task2_zscore),values_to="zscore"))[2]))
  })
  
  individualDataMark <- reactive({
    req(input$selectedStudent)
    pivot_longer((individualData()[1:2]),cols=starts_with("task"),values_to="mark")[2]
  })
  
  individualDataZScore <- reactive({
    req(input$selectedStudent)
    pivot_longer((individualData()[3:4]),cols=starts_with("task"),values_to="zscore")[2]
  })
  
  output$result <- renderText({
    paste(input$selectedStudent,"scored",data[data$name == input$selectedStudent,]$task1,"in task 1 and",data[data$name == input$selectedStudent,]$task2,"in task 2.")
  })

  output$plot1 <- renderPlot({
    g <- ggplot(data, aes(task1, task2, color = class, label = name)) + 
      geom_point(size=1.5) +
      geom_point(aes(x=data[data$name == input$selectedStudent,]$task1,y=data[data$name == input$selectedStudent,]$task2),colour="black",size=6,shape=9) +
      geom_abline(slope=1, color="black", linetype=1, size=.5) +
      xlim(0, 100) + 
      ylim(0, 100)
    g
  })
  output$plot2 <- renderPlot({
    h <- ggplot(data_scaled_names, aes(task1_zscore, task2_zscore, color = class, label = name)) + 
      geom_point(size=1.5) +
      geom_point(aes(x=data_scaled_names[data$name == input$selectedStudent,]$task1_zscore,y=data_scaled_names[data$name == input$selectedStudent,]$task2_zscore),colour="black",size=6,shape=9) +
      geom_abline(slope=1, color="black", linetype=1, size=.5)
    h
  })
  output$plot3 <- renderPlot({
    i <- ggplot(individualDataMark(), aes(x = as.numeric(row.names(individualDataMark())),y=mark), mark) +
      geom_bar(stat="identity", fill="#ABCDEF") +
      geom_line(data=individualDataZScore(), aes(x=as.numeric(row.names(individualDataMark())), y=(17.5*zscore+50)),stat="identity",color="red") +
      xlab("Task") +
      scale_y_continuous(
        limits = c(0,100),
        name = "Percentage",
        sec.axis = sec_axis(~./17.5-(50/17.5),name="Z-Score")
      ) +
      scale_x_continuous(
        labels = c(NA, 1, NA, 2, NA),
        n.breaks = 5
      )
    i
  })
}

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #CCCCCC;}"))
  ),
  
    titlePanel("Individual Result Comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("selectedClass", "Class", "Names"),
            selectInput("selectedStudent", "Student", "Names"),
            hr(),
            textOutput("result")
            ),
        mainPanel(
          tabsetPanel(
            tabPanel("Percantages", plotOutput("plot1")), 
            tabPanel("Z-Scores", plotOutput("plot2")),
            tabPanel("Historic", plotOutput("plot3"))
          )
        )
    )
)

shinyApp(ui = ui, server = server)
