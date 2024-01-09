library(stringr)
library(tidytext)
library(stopwords)
library(tm)
library(syuzhet)
library(dplyr)

#funcao

zero_ou_um <- function(x){
  ifelse(x == 0,x,1)
}
###########################


dados <- read.csv('base.csv')

lexiconPT::oplexicon_v2.1 |> 
  filter(grepl('me',term))

dados |> 
  unnest_tokens(output = word, input = mudanca_notavel) |>
  left_join(lexiconPT::oplexicon_v2.1,c('word'='term')) |> 
  filter(abs(polarity) == 1) |> 
  filter( !(word %in% stopwords::stopwords(language = "pt"))) |> 
  select(word,sentimento = polarity) |> 
  group_by(word,sentimento) |> 
  count() |> 
  ggplot(aes(y = reorder(word, +n), x = n, fill = sentimento)) +
  geom_bar(stat = "identity",width = 0.62, alpha = 0.7)

stopwords::stopwords(language = "pt")

dados |> 
  unnest_tokens(output = word, input = denovo_manter, token = "ngrams", n = 2) |> 
  filter(is.na(word) == F) |> 
  separate(word, c('word1','word2'), sep = " ") |> 
  filter(!(word1 %in% stopwords::stopwords(language = "pt"))) |> 
  filter(!(word2 %in% stopwords::stopwords(language = "pt"))) |> 
  unite(word, word1, word2, sep = " ") |> 
  count(word, sort = T) |> 
  slice(1:15) |>
  ggplot(aes(reorder(word,+n),n))+
  geom_bar(stat = 'identity',width = 0.62, alpha = 0.7)+
  coord_flip() 



dados |> 
  unnest_tokens(output = word, input = denovo_manter, token = "ngrams", n = 3) |> 
  filter(is.na(word) == F) |> 
  separate(word, c('word1','word2','word3'), sep = " ") |> 
  filter(!(word1 %in% stopwords::stopwords(language = "pt"))) |> 
  filter(!(word2 %in% stopwords::stopwords(language = "pt"))) |> 
  filter(!(word3 %in% stopwords::stopwords(language = "pt"))) |> 
  unite(word, word1, word2, word3, sep = " ") |> 
  count(word, sort = T) |> 
  slice(1:15) |>
  ggplot(aes(reorder(word,+n),n))+
  geom_bar(stat = 'identity',width = 0.62, alpha = 0.7)+
  coord_flip() 





dados |> 
  unnest_tokens(output = word, input = ambiente_aberta, token = "ngrams", n = 2) |> 
  filter(is.na(word) == F) |> 
  separate(word, c('word1','word2'), sep = " ") |> 
  filter(!(word1 %in% stopwords::stopwords(language = "pt"))) |> 
  filter(!(word1 %in% stopwords::stopwords(language = "pt"))) |> 
  unite(word, word1, word2, sep = " ") |> 
  count(word, sort = T) |> 
  slice(1:15) |>
  ggplot(aes(reorder(word,+n),n))+
  geom_bar(stat = 'identity',width = 0.62, alpha = 0.7)+
  coord_flip() 



dados |> 
  unnest_tokens(output = word, input = denovo_manter, token = "ngrams", n = 3) |> 
  filter(is.na(word) == F) |> 
  separate(word, c('word1','word2','word3'), sep = " ") |> 
  filter(!(word1 %in% stopwords::stopwords(language = "pt"))) |> 
  filter(!(word2 %in% stopwords::stopwords(language = "pt"))) |> 
  filter(!(word3 %in% stopwords::stopwords(language = "pt"))) |> 
  unite(word, word1, word2, word3, sep = " ") |> 
  count(word, sort = T) |> 
  slice(1:15) |>
  ggplot(aes(reorder(word,+n),n))+
  geom_bar(stat = 'identity',width = 0.62, alpha = 0.7)+
  coord_flip() 















# encontrando sinonimos

# Exemplo de respostas
respostas <- c("organização do ambiente", 
               "lugar arrumado", 
               "banheiros limpos e cheirosos")

# Criar um corpus de texto
corpus <- Corpus(VectorSource(respostas))

# Funções de pré-processamento
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
corpus <- tm_map(corpus, stripWhitespace)

# Visualizar o corpus após pré-processamento
inspect(corpus)
# Lista de sinônimos para "organização boa"
sinonimos_organizacao <- c("organizaçao", "arrumado", "limpo", "cheiroso")

# Função para identificar sinônimos e agrupar respostas
identificar_sinonimos <- function(text, sinonimos) {
  features <- unlist(strsplit(as.character(text), " "))
  sum(sapply(features, function(word) word %in% sinonimos)) > 0
}

# Aplicar a função no corpus
respostas_agrupadas <- sapply(corpus, identificar_sinonimos, sinonimos = sinonimos_organizacao)

# Exibir resultados
print(respostas_agrupadas)


# Calcular sentimentos para cada resposta
sentimentos <- sapply(corpus, function(resposta) {
  sentiment <- get_sentiment(resposta, method = "afinn")
  return(sentiment)
})

# Adicionar sentimentos ao data frame de respostas
df_respostas <- data.frame(Resposta = respostas, Sentimento = sentimentos)

# Exibir as respostas e seus sentimentos
print(df_respostas)

# Análise para recomendações
recomendacoes <- df_respostas %>%
  group_by(Sentimento) %>%
  summarise(Contagem = n())

# Se houver sentimentos negativos relacionados à organização, recomendar ação
if(any(recomendacoes$Sentimento < 0)) {
  print("Recomendamos investir em treinamento de atendimento e organização do ambiente.")
}

#####################################
# Instalar e carregar o pacote stringdist
library(stringdist)

# Respostas do questionário
respostas <- c("organização do ambiente",
               "lugar arrumado", "banheiros limpos e cheirosos")

# Calcular a matriz de distância entre as respostas
distancias <- stringdistmatrix(respostas, respostas, method = "jaccard")

# Aplicar um threshold para considerar respostas como similares
threshold <- 0.5  # Este valor pode ser ajustado conforme necessidade
respostas_similares <- distancias < threshold

# Exibir respostas similares
print(respostas_similares)




# Instalar e carregar o pacote text2vec
install.packages("text2vec")
library(text2vec)


# Exemplo de uso do text2vec para calcular word embeddings e encontrar respostas similares
# Este código é apenas um esqueleto e requer mais detalhes baseados em seus dados específicos

# Pré-processamento das respostas
tokens <- word_tokenizer(respostas)
it <- itoken(tokens, progressbar = FALSE)
vocabulary <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocabulary)
dtm <- create_dtm(it, vectorizer)

# Calcular word embeddings (este é um exemplo simplificado)
word_vectors <- GlobalVectors$new(rank = 50, x_max = 10)
word_vectors$fit_transform(dtm, n_iter = 10)

# Encontrar respostas similares
# ... (Código para calcular similaridades e agrupar respostas)

library(wordcloud2)



wordcloud2(demoFreq)
wordcloud2(demoFreq, size = 2)


################

#atnd_experience = sample(qualidade_atendimento, n, replace = TRUE),
#ambiente_aberta = sample(ambiente_respostas, n, replace = TRUE)

dados |> 
  unnest_tokens(output = word, input = ambiente_aberta) |>
  left_join(lexiconPT::oplexicon_v2.1,c('word'='term')) |> 
  filter(abs(polarity) == 1) |> 
  filter( !(word %in% stopwords::stopwords(language = "pt"))) |> 
  select(word,sentimento = polarity) |> 
  group_by(word,sentimento) |> 
  count() |> 
  ggplot(aes(y = reorder(word, +n), x = n, fill = sentimento)) +
  geom_bar(stat = "identity",width = 0.62, alpha = 0.7)

#novo_diferente = sample(c(novidade_respostas,diferente_respostas), n, replace = TRUE),
dados |> 
  unnest_tokens(output = word, input = novo_diferente) |>
  left_join(lexiconPT::oplexicon_v2.1,c('word'='term')) |> 
  filter(!is.na(polarity)) |> 
  filter( !(word %in% stopwords::stopwords(language = "pt"))) |> 
  select(word,sentimento = polarity) |> 
  group_by(word,sentimento) |> 
  count() |> 
  ungroup() |> 
  slice(1:15) |> 
  ggplot(aes(y = reorder(word, +n), x = n, fill = sentimento)) +
  geom_bar(stat = "identity",width = 0.62, alpha = 0.7)

#quando vier de novo o que quer rever?
#denovo_manter = sample(igual, n, replace = TRUE),
dados |> 
  unnest_tokens(output = word, input = denovo_manter) |>
  left_join(lexiconPT::oplexicon_v2.1,c('word'='term')) |> 
  #filter(abs(polarity) == 1) |> 
  filter( !(word %in% stopwords::stopwords(language = "pt"))) |> 
  filter(str_sub(word,start = str_length(word)-1) != "ia") |> 
  select(word,sentimento = polarity) |> 
  group_by(word,sentimento) |> 
  count() |> 
  ggplot(aes(y = reorder(word, +n), x = n, fill = sentimento)) +
  geom_bar(stat = "identity",width = 0.62, alpha = 0.7)+
  geom_text(
    aes(x = n, label = n),
    hjust = 1
  )+
  labs(y = '',x = 'número de respostas')
#cliente antigo, qual diferença mais te chamou atenção?
#possibilidade de trocar por "o que faria vc vir de novo"
mudanca_notavel = sample(diferencas_notaveis_respostas, n, replace = TRUE),
flag_cliente_novo = sample(0:1, n, replace = TRUE,prob = c(0.8,0.2)),
Date_Time = sample(seq(as.POSIXct('2023-11-01'), as.POSIXct('2023-12-31'), by="hour"),
                   n, replace = T)





# este código visa certificar-se de que cada palavra da lista (rankeada da mais frequente para a menos frequente),
# não está significantemente representada por alguma outra palavra que está acima no ranking.
#  caso isso aconteça, a palavra alvo é excluida.


aux <- dados |> 
  unnest_tokens(output = word, input = ambiente_aberta) |>
  left_join(lexiconPT::oplexicon_v2.1,c('word'='term')) |> 
  filter(!is.na(polarity)) |> 
  filter( !(word %in% stopwords::stopwords(language = "pt"))) |> 
  select(word,sentimento = polarity) |> 
  group_by(word,sentimento) |> 
  count()


corpus <- Corpus(VectorSource(dados$ambiente_aberta))
dtm <- TermDocumentMatrix(corpus)

str_remove(dtm$dimnames$Terms,"\\,|\\.")
tibble_termdocmat <- as.matrix(dtm) |> 
  as_tibble(rownames = "palavras") |>  
  mutate(across(2:(dim(dtm)[2]+1),zero_ou_um)) |> 
  filter(str_remove(palavras,"\\,|\\.") %in% aux$word)


representado <- list()


for(i in 2:nrow(aux)){
  flag_representado <- numeric()
  for(j in 1:(i-1)){
    palavra_alvo_presente <- tibble_termdocmat |> 
      slice(c(i)) |> select(-1) |> colSums()
    
    ambos_presentes <- tibble_termdocmat |> 
      slice(c(j,i)) |> select(-1) |> colSums()

    flag_representado[j] <- sum(which(ambos_presentes == 2) == which(palavra_alvo_presente == 1))/sum(palavra_alvo_presente == 1)

  }
  
  representado[[i-1]] <- flag_representado
}



