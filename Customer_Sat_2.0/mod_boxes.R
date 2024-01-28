library(shiny)
library(ggplot2)

box_ui <- function(id){
  ns <- NS(id)
    valueBoxOutput(ns("box_cliente"), width = 6)
}

box_server <- function(id,df,clientes_novos = FALSE){
  
  moduleServer(id = id,
               module = function(input,output,session){
                
                 if(clientes_novos){
                   
                   clientes_filtrados <- reactive({ 
                     df() |> 
                       filter(flag_cliente_novo == 1)
                        })
                   box_title <- "CLIENTES NOVOS"
                   cor <- "aqua"
                   
                   }else{
                   
                     clientes_filtrados <- reactive({ 
                       df()
                       })
                     box_title <- "TOTAL CLIENTES"
                     cor <- "blue"
                     
                 }  

                  
                 output$box_cliente <- renderValueBox({
                   valueBox(nrow(clientes_filtrados()), box_title,
                            icon = icon("users"),color = cor
                   )
                 })
                 
                 }
               )
}