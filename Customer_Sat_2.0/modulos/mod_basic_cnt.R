library(shiny)
library(ggplot2)

basic_cnt_ui <- function(id,box_title){
  
  ns <- NS(id)
    
    box(title = box_title,
        plotOutput(ns("basic")), 
        width = 6, status = "primary",
        solidHeader = T)
  
}


basic_cnt_server <- function(id,df,x_col){
  moduleServer(
    id = id,
    module = function(input,output,session){
      
      output$basic <- renderPlot({
        
        df() |> 
        select(x = all_of(x_col)) |> 
        ggplot( aes(x= x)) + 
        geom_bar()+
        theme_ipsum()+
        labs(y = '',x = 'número de respostas')
        
      })
    }
  )
}

