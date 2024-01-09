library(shiny)
library(ggplot2)

# Assuming 'survey_data' is your dataset
 survey_data <- read.csv("dados.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Customer Satisfaction Analysis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select the analysis you want to view:")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Overall Satisfaction", plotOutput("satisfactionPlot")),
        tabPanel("Food Quality", plotOutput("foodPlot")),
        tabPanel("Service Quality", plotOutput("servicePlot")),
        tabPanel("First Time Customers", plotOutput("firstTimePlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$satisfactionPlot <- renderPlot({
    ggplot(survey_data, aes(x=factor(Overall_Satisfaction))) +
      geom_bar() +
      labs(title="Overall Satisfaction Ratings", x="Rating", y="Count")
  })
  
  output$foodPlot <- renderPlot({
    ggplot(subset(survey_data, !is.na(Specific_Dish_Rating)), aes(x=factor(Specific_Dish_Rating))) +
      geom_bar() +
      labs(title="Ratings for Dish #2", x="Rating", y="Count")
  })
  
  output$servicePlot <- renderPlot({
    ggplot(survey_data, aes(x=factor(Service_Quality))) +
      geom_bar() +
      labs(title="Service Quality Ratings", x="Rating", y="Count")
  })
  
  output$firstTimePlot <- renderPlot({
    ggplot(survey_data, aes(x=factor(First_Time_Customer), fill=factor(Overall_Satisfaction))) +
      geom_bar(position="dodge") +
      labs(title="First Time vs Regular Customers", x="First Time Customer", y="Count")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
