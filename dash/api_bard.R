library(httr)
library(jsonlite)
library(dplyr)
# Function
gemini <- function(prompt, 
                   temperature=0.5,
                   max_output_tokens=1024,
                   api_key=Sys.getenv("gemini_app_secret"),
                   model = "gemini-pro") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Status Code - ", response$status_code))
  }
  
  candidates <- httr::content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}


dados <- read.csv('dash2/base.csv')

#o que o cliente gostaria de ver diferente quando visitar de novo
diferente_respostas <- unique(dados$novo_diferente)

respostas_treated <- diferente_respostas[stringr::str_length(diferente_respostas) > 0]

prompt <- "atue como um analista de pesquisa de satisfação.
vou lhe passar uma lista de respostas dos clientes,
onde eles dizem o que gostariam de ver numa próxima visita, 
analise as respostas e retorne ações para a equipe do restaurante.
abaixo vou enumerar algumas informações importantes 

- 1. a pergunta feita foi <o que você gostaria que fosse diferente numa próxima visita?> 
- 2. existe uma plataforma de treinamento de staff, caso sua analise identifique alguma necessidade de treinamento de equipe, identifique e informe qual treinamento é necessário   
- 3. a entrada é texto, mas a saída deve ser em HTML, mas não coloque dentro de um chunck

"

prompt <- paste(prompt,paste(diferente_respostas,collapse = '\n outro cliente: '))
retorno_gemini <- gemini(prompt)

write.table(retorno_gemini, file = "dash2/retorno_ia.txt", row.names = FALSE, col.names = FALSE)


########## abaixo, exemplo de um retorno MUITO bom #########
#pode ser usado para ensinar a IA como retornar coisas caso ela comece 
#a capengar em algum momento


# Análise de Pesquisa de Satisfação
# Tempo de espera para ser atendido: Muitos clientes reclamaram do tempo de espera para serem atendidos. Isso pode ser devido a uma série de fatores, como falta de pessoal, problemas na cozinha ou má gestão do fluxo de clientes. É importante que a equipe do restaurante tome medidas para reduzir o tempo de espera, como contratar mais funcionários, melhorar a eficiência da cozinha e implementar um sistema de reservas online.
# Fotos dos pratos no cardápio: Alguns clientes solicitaram que o cardápio tivesse mais fotos dos pratos. Isso pode ajudar os clientes a escolherem seus pratos com mais facilidade e também pode aumentar o apetite. A equipe do restaurante deve considerar adicionar fotos dos pratos ao cardápio.
# Mais opções de bebidas não alcoólicas: Alguns clientes pediram mais opções de bebidas não alcoólicas. Isso pode ser devido a uma série de fatores, como preferências pessoais, restrições alimentares ou simplesmente o desejo de uma bebida refrescante. A equipe do restaurante deve considerar adicionar mais opções de bebidas não alcoólicas ao menu, como sucos naturais, chás gelados e refrigerantes.
# Reservas online: Alguns clientes solicitaram um sistema de reservas online. Isso pode tornar mais fácil para os clientes reservarem uma mesa, especialmente durante horários de pico. A equipe do restaurante deve considerar implementar um sistema de reservas online.
# Acústica do local: Alguns clientes reclamaram do ruído do local. Isso pode ser devido a uma série de fatores, como o design do restaurante, o tipo de música que está sendo tocada ou o número de clientes. A equipe do restaurante deve considerar tomar medidas para melhorar a acústica do local, como instalar painéis acústicos ou reduzir o volume da música.
# Treinamento de Equipe
# Atendimento ao cliente: A equipe do restaurante deve receber treinamento em atendimento ao cliente para melhorar a experiência dos clientes. Isso inclui treinamento sobre como lidar com reclamações, como resolver problemas e como fornecer um serviço excelente.
# Eficiência da cozinha: A equipe da cozinha deve receber treinamento sobre como preparar os pratos de forma rápida e eficiente. Isso ajudará a reduzir o tempo de espera para os clientes.
# Gestão do fluxo de clientes: A equipe do restaurante deve receber treinamento sobre como gerenciar o fluxo de clientes de forma eficaz. Isso ajudará a evitar longas filas e tempos de espera.
# 


############### prompt for resposta aberta sobre ambiente ##########################


prompt_ambiente0 <- c("vou lhe passar um texto onde cada linha representa a resposta de um cliente à pergunta <o que mais te chamou atenção no ambiente do restaurante>, quero reduzir estas respostas, através de tags, uma tag deve ser uma string de até 3 palavras separadas por espaço, que consiga representar a mensagem passada pelo texto que ela representa. Dito isto, encontre a palavra chave de cada resposta e retorne a contagem de tag e se o sentimento daquela mensagem é positivo ou negativo. Não quero que me mostre nenhum código, quero que execute a atividade. a saída deve ter letras minusculas e vir de maneira que eu possa inserir dentro da função fread e com isso retornar um dataframe sem header e com 3 colunas : Tag, Contagem, Sentimento

respostas
")

ambiente_respostas <- dados$ambiente_aberta

prompt_add_tag_externa <- c("associar os dados a tags já existentes deve ser priorizado, mas se preciso, pode criar outras tags")
tags_existentes <- "teste teste"

retorno_df <- data.table::data.table()
#comprimento do loop
k <- ceiling(nrow(dados)/50)

for(i in 1:k){

if(i < k){
  indice_inicio <- i*50 - 49
  indice_fim <- i*50
}else{
  indice_inicio <- (i-1)*50 + 1
  indice_fim <- nrow(dados)
  }
prompt_ambiente <- paste(prompt_ambiente0,paste(ambiente_respostas[indice_inicio:indice_fim],
                                                collapse = '\n outro cliente: '))
retorno_gemini <- gemini(prompt_ambiente)

retorno_aux <- data.table::fread(retorno_gemini) 

if(!any(grepl("tag",names(retorno_df)))){
  names(retorno_aux) <- c('tag','contagem','sentimento')
}

retorno_df <- retorno_df |> 
bind_rows(retorno_aux)


}

retorno_df <- retorno_df |> 
  group_by(tag, sentimento) |> 
  summarise(contagem = sum(contagem))


write.csv(retorno_df,'resposta_aberta_ambiente.csv')



############### prompt for resposta aberta sobre o que gostaria de ver diferente ##########################


prompt_ambiente0 <- c("vou lhe passar um texto onde cada linha representa a resposta de um cliente à pergunta <o que gostaria que fosse diferente numa segunda visita?>, quero reduzir estas respostas, através de tags, uma tag deve ser uma string de exatamente duas palavras separadas por underline, que consiga representar a mensagem passada pelo texto que ela representa. Dito isto, encontre a palavra chave de cada resposta e retorne a contagem de tag. Não quero que me mostre nenhum código, quero que execute a atividade. a saída deve ter letras minusculas e vir de maneira que eu possa inserir dentro da função fread e com isso retornar um dataframe sem header e com 2 colunas : Tag, Contagem

respostas
")

ambiente_respostas <- dados$novo_diferente

prompt_add_tag_externa <- c("associar os dados a tags já existentes deve ser priorizado, mas se preciso, pode criar outras tags")
tags_existentes <- "teste teste"

retorno_df <- data.table::data.table()
#comprimento do loop
k <- ceiling(nrow(dados)/50)

for(i in 1:k){
  
  if(i < k){
    indice_inicio <- i*50 - 49
    indice_fim <- i*50
  }else{
    indice_inicio <- (i-1)*50 + 1
    indice_fim <- nrow(dados)
  }
  prompt_ambiente <- paste(prompt_ambiente0,paste(ambiente_respostas[indice_inicio:indice_fim],
                                                  collapse = '\n outro cliente: '))
  retorno_gemini <- gemini(prompt_ambiente)
  
  retorno_aux <- data.table::fread(retorno_gemini) 
  
  if(!any(grepl("tag",names(retorno_df)))){
    names(retorno_aux) <- c('tag','contagem')
  }
  
  retorno_df <- retorno_df |> 
    bind_rows(retorno_aux)
  
  
}

retorno_df <- retorno_df |> 
  group_by(tag) |> 
  summarise(contagem = sum(contagem))


write.csv(retorno_df,'diferente_visita2.csv')




############### prompt for resposta aberta sobre o que gostaria de rever ##########################


prompt_ambiente0 <- c("vou lhe passar um texto onde cada linha representa a resposta de um cliente à pergunta <o que gostaria de ver novamente numa segunda visita?>, quero reduzir estas respostas, através de tags, uma tag deve ser uma string de exatamente duas palavras separadas por underline, que consiga representar a mensagem passada pelo texto que ela representa. Dito isto, encontre a palavra chave de cada resposta e retorne a contagem de tag. Não quero que me mostre nenhum código, quero que execute a atividade. a saída deve ter letras minusculas e vir de maneira que eu possa inserir dentro da função fread e com isso retornar um dataframe sem header e com 2 colunas : Tag, Contagem

respostas
")

ambiente_respostas <- dados$denovo_manter

prompt_add_tag_externa <- c("associar os dados a tags já existentes deve ser priorizado, mas se preciso, pode criar outras tags")
tags_existentes <- "teste teste"

retorno_df <- data.table::data.table()
#comprimento do loop
k <- ceiling(nrow(dados)/50)

for(i in 1:k){
  
  if(i < k){
    indice_inicio <- i*50 - 49
    indice_fim <- i*50
  }else{
    indice_inicio <- (i-1)*50 + 1
    indice_fim <- nrow(dados)
  }
  prompt_ambiente <- paste(prompt_ambiente0,paste(ambiente_respostas[indice_inicio:indice_fim],
                                                  collapse = '\n outro cliente: '))
  retorno_gemini <- gemini(prompt_ambiente)
  
  retorno_aux <- data.table::fread(retorno_gemini) 
  
  if(!any(grepl("tag",names(retorno_df)))){
    names(retorno_aux) <- c('tag','contagem')
  }
  
  retorno_df <- retorno_df |> 
    bind_rows(retorno_aux)
  
  
}

retorno_df <- retorno_df |> 
  group_by(tag) |> 
  summarise(contagem = sum(contagem))


write.csv(retorno_df,'rever_visita2.csv')




# para testar a função post 
# POST(
#   url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", paste0("gemini-pro", ":generateContent")),
#   query = list(key = Sys.getenv("gemini_app_secret")),
#   content_type_json(),
#   encode = "json",
#   body = list(
#     contents = list(
#       parts = list(
#         list(text = "ola mundo")
#       )),
#     generationConfig = list(
#       temperature = 0.5,
#       maxOutputTokens = 1024
#     )
#   )
# )


