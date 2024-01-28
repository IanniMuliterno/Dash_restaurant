library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(stringr)
library(tidytext)
library(stopwords)
library(tm)
library(syuzhet)
library(httr)
library(jsonlite)

source("mod_boxes.R")
source("mode_ts.R")

dados <- read.csv('base.csv')|> 
  mutate(dia_semana = factor(dia_semana, levels = c('domingo','segunda-feira','terça-feira','quarta-feira',
                                                    'quinta-feira','sexta-feira','sábado')))


RAA_df <- read.csv('resposta_aberta_ambiente.csv')
DV2_df <- read.csv('diferente_visita2.csv')
RV2_df <- read.csv('rever_visita2.csv')
read_text <- read.table("retorno_ia.txt", header = FALSE, stringsAsFactors = FALSE)
retorno_ia <- read_text$V1

# UI
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Customer Satisfaction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Analytics", tabName = "analytics", icon = icon("chart-line"))
      # Add additional menu items as needed
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              column(width = 3, offset = 3,
                     selectInput(
                       "mes_selecionado", "Mês",
                       choices = unique(dados$mes),
                       multiple = T,
                       selectize = T,
                       selected = unique(dados$mes)
                     )
              ),
              column(width = 3,
                     selectInput(
                       "diasemana_selecionado", "Dia da semana",
                       choices = unique(dados$dia_semana),
                       multiple = T,
                       selectize = T,
                       selected = unique(dados$dia_semana)
                     )
              ),
              column(width = 3,
                     selectInput(
                       "hora_selecionado", "Hora inicial",
                       choices = unique(dados$hora),
                       multiple = T,
                       selectize = T,
                       selected = unique(dados$hora)
                     )
              ),
              fluidRow(
                box_ui("box_total_cli"),
                box_ui("box_novo_cli")
                ),
              ts_ui("flow")
              
              
      )
)
))


server <- function(input, output,session) {
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    if(!is.null(input$mes_selecionado) &
       !is.null(input$diasemana_selecionado) &
       !is.null(input$hora_selecionado)){
      filter(dados, 
             mes %in% input$mes_selecionado,
             dia_semana %in% input$diasemana_selecionado,
             hora %in% input$hora_selecionado)
    }else{dados}
  })
  
  box_server(id = "box_total_cli",
             df = filtered_data, 
             clientes_novos = FALSE)
  
  box_server(id = "box_novo_cli",
             df = filtered_data, 
             clientes_novos = TRUE)
  
  ts_server(id = "flow",
            df = filtered_data)

  
}

# Run the app
shinyApp(ui, server)
