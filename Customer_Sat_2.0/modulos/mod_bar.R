library(shiny)
library(ggplot2)

bar_ui <- function(id,box_title = "title"){
  ns <- NS(id)
  box(title = box_title,
      plotOutput(ns("bar_chart")), 
      width = 6, status = "primary",
      solidHeader = T)
}


bar_server <- function(id,df){
  moduleServer(
    id = id,
    module = function(input,output,session){
      
      output$bar_chart <- renderPlot({
        df |>  
        arrange(desc(contagem)) |> 
        slice(1:5) |>
        ggplot(aes(reorder(tag,+contagem),contagem,fill = sentimento))+
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