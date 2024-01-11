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


# If you need to create maps, you might need to install and load 'leaflet' or 'highcharter'
dados <- read.csv('base.csv')

diferente_respostas <- c(
  "Espero que o tempo de espera para ser atendido seja menor.",
  "Gostaria que o cardápio tivesse mais fotos dos pratos.",
  "",
  "Preferiria que houvesse mais opções de bebidas não alcoólicas.",
  "Seria melhor se o restaurante aceitasse reservas online.",
  "",
  "Poderiam melhorar a acústica do local para reduzir o ruído.",
  ""
)
respostas_treated <- diferente_respostas[stringr::str_length(diferente_respostas) > 0]

prompt <- "atue como um analista de pesquisa de satisfação.
vou lhe passar uma lista de respostas dos clientes,
onde eles dizem o que gostariam de ver numa próxima visita, 
analise as respostas e retorne ações para a equipe do restaurante.
abaixo vou enumerar algumas informações importantes 

- 1. a pergunta feita foi <o que você gostaria que fosse diferente numa próxima visita?> 
- 2. existe uma plataforma de treinamento de staff, caso sua analise identifique alguma necessidade de treinamento de equipe, identifique e informe qual treinamento é necessário   

aqui estão as respostas dos clientes

"

prompt <- paste(prompt,paste(diferente_respostas,collapse = 'outro cliente: '))

# UI
# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Enterprise Dashboard"),
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
              fluidRow(
                valueBoxOutput("total_cliente", width = 6),
                valueBoxOutput("productionCosts", width = 6)
              ),
              fluidRow(
                box(title = "Fluxo de respostas",plotOutput("fluxo"), width = 12, status = "primary",
                    solidHeader = T)
              ),
              fluidRow(
                box(title = "Qualidade do prato", status = "primary", solidHeader = TRUE,
                  #  "Box content here", br(), "More box content",
                    plotOutput("revenueBreakdown"), width = 6),
                box(title = "Recomendações",
                    HTML(retorno_gemini)  ,
                    width = 6, status = "primary",
                    solidHeader = T)
              )
      ),
      tabItem(tabName = "analytics",
              fluidRow(
                box(title = "Resposta aberta sobre ambiente",
                    plotOutput("ambiente_aberta"), width = 6, status = "primary",
                    solidHeader = T),
                box(title = "Experiência no atendimento",
                    plotOutput("atend_experience"), width = 6, status = "primary",
                    solidHeader = T)
              ),
              fluidRow(
                box(title = "O que gostaria de ver diferente numa segunda visita",
                    plotOutput("visita2_diferente"), width = 6, status = "primary",
                    solidHeader = T),
                box(title = "O que gostaria de ver novamente numa segunda visita",
                    plotOutput("denovo_manter"), width = 6, status = "primary",
                    solidHeader = T)
              )
              
      )
    )
  )
)


server <- function(input, output) {
  output$total_cliente <- renderValueBox({
    valueBox(nrow(dados), "TOTAL CLIENTES", icon = icon("users"),color = "blue"
    )
  })
  
  output$productionCosts <- renderValueBox({
    valueBox(sum(dados$flag_cliente_novo), "CLIENTES NOVOS", icon = icon("users"), color = "aqua"
    )
  })
  

  
  output$revenueBreakdown <- renderPlot({
    ggplot(dados, aes(x=Food_Quality)) + geom_bar()+
      theme_ipsum()
  })
  
  output$fluxo <- renderPlot({
    dados |> 
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
  
  output$ambiente_aberta <- renderPlot({
    dados |> 
      unnest_tokens(output = word, input = ambiente_aberta, token = "ngrams", n = 2) |> 
      filter(is.na(word) == F) |> 
      filter(str_sub(word,start = str_length(word)-1) != "ia") |> 
      #filter(type != 'vb') |> 
      separate(word, c('word1','word2'), sep = " ") |> 
      filter(!(word1 %in% stopwords::stopwords(language = "pt"))) |> 
      filter(!(word2 %in% stopwords::stopwords(language = "pt"))) |> 
      unite(word, word1, word2, sep = " ") |> 
      count(word, sort = T) |> 
      slice(1:15) |>
      ggplot(aes(reorder(word,+n),n))+
      geom_bar(stat = 'identity',width = 0.62, alpha = 0.7)+
      coord_flip() +
      geom_text(
        aes(y = n, label = n),
        hjust = 1
      )+
      labs(y = '',x = 'número de respostas')
  })
  
  output$atend_experience <- renderPlot({
    dados |> 
      filter(atnd_experience != 'comente') |> 
      ggplot( aes(x=atnd_experience)) + geom_bar()+
      theme_ipsum()+
      labs(y = '',x = 'número de respostas')
  })
  
  output$visita2_diferente <- renderPlot({
    dados |> 
      unnest_tokens(output = word, input = novo_diferente) |>
      left_join(lexiconPT::oplexicon_v2.1,c('word'='term')) |> 
      filter(!is.na(polarity)) |> 
      filter( !(word %in% stopwords::stopwords(language = "pt"))) |>
      filter(str_sub(word,start = str_length(word)-1) != "ia") |> 
      filter(type != 'vb') |> 
      select(word,sentimento = polarity) |> 
      group_by(word,sentimento) |> 
      count() |> 
      ungroup() |> 
      slice(1:15) |> 
      ggplot(aes(y = reorder(word, +n), x = n, fill = sentimento)) +
      geom_bar(stat = "identity",width = 0.62, alpha = 0.7)+
      geom_text(
        aes(x = n, label = n),
        hjust = 1
      )+
      labs(y = '',x = 'número de respostas')
  })
  
  output$denovo_manter <- renderPlot({
    dados |> 
      unnest_tokens(output = word, input = denovo_manter) |>
      left_join(lexiconPT::oplexicon_v2.1,c('word'='term')) |> 
      filter(!is.na(polarity)) |> 
      filter( !(word %in% stopwords::stopwords(language = "pt"))) |> 
      filter(str_sub(word,start = str_length(word)-1) != "ia") |> 
      filter(type != 'vb') |> 
      select(word,sentimento = polarity) |> 
      group_by(word,sentimento) |> 
      count() |> 
      ungroup() |> 
      slice(1:15) |> 
      ggplot(aes(y = reorder(word, +n), x = n, fill = sentimento)) +
      geom_bar(stat = "identity",width = 0.62, alpha = 0.7)+
      geom_text(
        aes(x = n, label = n),
        hjust = 1
      )+
      labs(y = '',x = 'número de respostas')
  })

}

# Run the app
shinyApp(ui, server)
