library(shiny)
library(readr)
library(tidyverse)

ui <- fluidPage(
  
  titlePanel("Companie's Average Wage Graphing"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(inputId = "Company_code", label = "Imones kodas",
                     choices = NULL, selected = NULL)),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server = function(input, output, session) {
  duom = read.csv("data/lab_sodra.csv")
  data = duom %>%
    filter(ecoActCode == 494100) %>%
    mutate(month_amt=as.integer(substr(month, 5 ,7)))
  
  kodai = data %>%
    group_by(name) %>%
    summarise(code = max(code))
  
  updateSelectizeInput(session, "Company_code",
                       choices = kodai$code,
                       server = TRUE)
  
  output$distPlot = renderPlot({
    data %>%
      filter(code == input$Company_code) %>%
      ggplot(aes(x = month_amt, y = avgWage)) +
      scale_x_continuous("month", breaks = 1:12, limits = c(1,12)) +
      theme_grey() +
      geom_line(colour = 'red')+
      labs(x = "Month", y = "Average Wage")
  })
}

shinyApp(ui = ui, server = server)