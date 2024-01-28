library(shiny)
library(ggplot2)

ts_ui <- function(id){
  ns <- NS(id)
  fluidRow(
    box(title = "Fluxo de respostas",
        plotOutput(ns("fluxo")),
        width = 12, status = "primary",
        solidHeader = T)
  )
}

ts_server <- function(id,df){
  
  moduleServer(
  id = id,
  module = function(input,output,session){
    
    output$fluxo <- renderPlot({
      received_data <- df()
      received_data |> 
        mutate(data2 = stringr::str_sub(Date_Time,1,10)) |> 
        group_by(data2) |> 
        count() |>
        tail(30) |> 
        ggplot(aes(x = data2, y = n,group = 1)) +
        geom_line()+
        geom_point(shape=21, color="black", fill="#00FFFF", size=6) +
        theme_ipsum() +
        theme(axis.text.x = element_text(angle = 45))
    })
    
  }
  )
}