library(httr)
library(jsonlite)

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
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}


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

prompt <- "vou lhe passar uma lista de respostas dos clientes, onde eles dizem o que gostariam de ver numa próxima visita, resuma isso em ações para a equipe do restaurante, por exemplo. 'os clientes gostariam de atendimento mais rápido, consulte na nossa plataforma o treinamento de comunicação eficiente e gestão de tempo' um outro exemplo poderia ser: 'muitos clientes falaram que gostariam de ver mais opções veganas', no fim dos insights você mesmo pode dar sugestões de perguntas que a gestão do restaurante poderia fazer para você, para ter mais insights que aumentem seu lucro. Por exemplo, caso haja algum insight sobre clientes pedindo mais opções veganas, você pode responder sugerindo que a gerência consulte a concorrência na região, pois se houverem poucos restaurantes veganos, a ideia pode ser mais lucrativa

aqui estão as respostas dos clientes

"

prompt <- paste(prompt,paste(diferente_respostas,collapse = 'outro cliente: '))
cat(gemini(prompt))
