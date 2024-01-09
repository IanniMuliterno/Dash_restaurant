set.seed(2023) # For reproducibility

# Define the number of rows you want in the dataset
n <- 645 # Replace 100 with the number of rows you need
qualidade_atendimento <- c('tem atencao',
'habilidosos',
'gente boa',
'organização',
'comente')
# se pudesse escolher um ou mais pontos que mais te chamaram atenção no ambiente? 
#O que escolheria? caixinha embaixo com "comente sua escolha"
ambiente <- c("estacionamento",
                   "conforto",
                   "instagramavel",
                   "temperatura",
                   "acessibilidade")
# + aberta

# Sample answers for "What most caught your attention in the restaurant's environment?"
ambiente_respostas <- c(
  "O estilo moderno e acolhedor do interior.",
  "",
  "A música ambiente estava perfeita, não muito alta nem muito baixa.",
  "A iluminação suave e as cores harmoniosas do espaço.",
  "",
  "A limpeza e organização do local foram impecáveis.",
  "O sorriso e a simpatia dos funcionários.",
  ""
)



 novidade_respostas <- c(
   "Gostaria de ver opções de pratos veganos no menu.",
   "Seria ótimo ter uma área para crianças brincarem.",
   "",
   "Adoraria ver mais opções de sobremesas sem glúten.",
   "",
   "Uma noite temática por semana seria interessante.",
   "Mais variedade de cervejas artesanais.",
   ""
 )
 
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
 #o que gostaria de ver igual
igual <- c("adorei o milkshake, tem sabores diferentes, voltaria só por ele",
           "",
           "organização do ambiente",
           "lugar arrumado",
           "facil acesso",
           "",
           "banheiros cheirosos",
           "as sobremesas são maravilhosas, nunca mudem",
           "",
           "o milkshake, eu viria só por ele")
 #então aqui está um convite para gente se ver de novo.  
 
 ### para clientes mais antigos

# qual a mudança que mais te chamou atenção desde a sua ultima visita?
 diferencas_notaveis_respostas <- c(
   "A qualidade do serviço melhorou muito, os garçons estão mais atenciosos.",
   "Notei que o menu foi atualizado com mais opções saudáveis.",
   "",
   "O restaurante está com uma decoração nova, muito mais moderna e convidativa.",
   "A música ambiente mudou e agora está mais agradável.",
   "",
   "O tempo de espera pela comida diminuiu significativamente.",
   "As porções estão maiores, o que é ótimo para o preço que pagamos.",
   "Está mais organizado, especialmente a área de espera por mesas.",
   "",
   "Os preços estão mais acessíveis, o que me surpreendeu positivamente.",
   "Agora há mais opções de vinhos e coquetéis no cardápio.",
   "",
   "Percebi que o restaurante está usando ingredientes mais frescos e de melhor qualidade."
 )
 ############################

# Generate artificial data
survey_data <- data.frame(
  atnd_experience = sample(qualidade_atendimento, n, replace = TRUE),
  ambiente = sample(ambiente, n, replace = TRUE),
  ambiente_aberta = sample(ambiente_respostas, n, replace = TRUE),
  Food_Quality = sample(1:5, n, replace = TRUE,prob = c(0.1,0.2,0.25,0.3,0.15)),
  #quando vier de novo o que quer ver de diferente?
  novo_diferente = sample(c(novidade_respostas,diferente_respostas), n, replace = TRUE),
  #quando vier de novo o que quer rever?
  denovo_manter = sample(igual, n, replace = TRUE),
  #cliente antigo, qual diferença mais te chamou atenção?
  #possibilidade de trocar por "o que faria vc vir de novo"
  mudanca_notavel = sample(diferencas_notaveis_respostas, n, replace = TRUE),
  flag_cliente_novo = sample(0:1, n, replace = TRUE,prob = c(0.8,0.2)),
  Date_Time = sample(seq(as.POSIXct('2023-11-01'), as.POSIXct('2023-12-31'), by="hour"),
                     n, replace = T)
)

# View the first few rows of the dataset
head(survey_data)
write.csv(survey_data,'base.csv')
