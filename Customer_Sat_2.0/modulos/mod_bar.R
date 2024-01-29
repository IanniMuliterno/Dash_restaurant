library(shiny)
library(ggplot2)

bar_ui <- function(id,box_title = "title"){
  ns <- NS(id)
  box(title = box_title,
      plotOutput(ns("bar_chart")), 
      width = 6, status = "primary",
      solidHeader = T)
}


bar_server <- function(id,df,sentiment = FALSE){
  moduleServer(
    id = id,
    module = function(input,output,session){
      if(sentiment){
        df2 <- df |>  
          arrange(desc(contagem)) |> 
          slice(1:5) |>
          ggplot(aes(reorder(tag,+contagem),contagem,fill = sentimento))
        
      }else{
        df2 <- df |>  
          arrange(desc(contagem)) |> 
          slice(1:5) |>
          ggplot(aes(reorder(tag,+contagem),contagem))
        
      }
      
      
      output$bar_chart <- renderPlot({
        df2 +
        geom_bar(stat = 'identity',width = 0.62, alpha = 0.7) +
        coord_flip() +
        theme_ipsum() +
        scale_fill_brewer(palette="Paired") +
        geom_text(
          aes(y = contagem, label = contagem),
          hjust = 1
        )+
        labs(y = '',x = 'número de respostas')
      })
    }
    
  )
}