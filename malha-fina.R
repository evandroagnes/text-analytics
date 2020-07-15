library(dplyr)
library(qdap)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(ggthemes)
library(stringi)
library(RColorBrewer)
library(RWeka)

custom_stopwords <- c(stopwords("portuguese"), "malha", "fina", "receita", "federal", "receitafederal")

clean_corpus <- function(corpus, stops){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stops)
  corpus <- tm_map(corpus, content_transformer(stri_trans_general), 'Latin-ASCII')
  return(corpus)
}

# malha_fina_tudo <- read.csv("s4-154044-1.csv", stringsAsFactors = FALSE)
# malha_fina <- malha_fina_tudo
malha_fina <- read.csv("data/s4-154109-2.csv", stringsAsFactors = FALSE)

str(malha_fina)
malha_fina_text <- malha_fina$text
malha_fina_corp <- VCorpus(VectorSource(malha_fina_text))
malha_fina_clean <- clean_corpus(malha_fina_corp, custom_stopwords)
malha_fina_tdm <- TermDocumentMatrix(malha_fina_clean)
malha_fina_m <- as.matrix(malha_fina_tdm)

malha_fina_clean_s <- clean_corpus(malha_fina_corp, stopwords("portuguese"))

# total de menções utilizadas na análise  
nrow(malha_fina)

# total de mencões por mídia social
malha_fina %>%
  group_by(social_media) %>%
  summarize(count = n())

# total de menções por cidade (twitter)
malha_fina_cidade <- malha_fina %>%
  filter(!is.na(city), city != "") %>%
  group_by(city) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

malha_fina_cidade$city <- factor(malha_fina_cidade$city)

ggplot(malha_fina_cidade, aes(x = reorder(city, count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal()

# Periodo de monitoramento
min(malha_fina$created_at)
max(malha_fina$created_at)

termos_frequentes <- rowSums(malha_fina_m)
termos_frequentes <- sort(termos_frequentes, decreasing = TRUE)

palavras_frequentes <- data.frame(term = names(termos_frequentes), 
                                  num = termos_frequentes)

# Criando paleta de cores para nuvem
paleta <- brewer.pal(8, "Reds")
paleta <-paleta[-(1:2)]

# nuvem de palavras mais frequentes
wordcloud(palavras_frequentes$term, palavras_frequentes$num, max.words = 100, colors = paleta)

# cluster das palavras mais frequentes
malha_fina_tdm2 <- removeSparseTerms(TermDocumentMatrix(malha_fina_clean_s), 0.965)
malha_fina_tdm2
malha_fina_m2 <- as.matrix(malha_fina_tdm2)
malha_fina_df2 <- as.data.frame(malha_fina_m2)
malha_fina_dist <- dist(malha_fina_df2)
hc <- hclust(malha_fina_dist)
plot(hc)

# associações com as palavras mais comuns
assoc <- findAssocs(TermDocumentMatrix(malha_fina_clean_s), "cai", 0.2)
assoc
assoc_df <- list_vect2df(assoc)[, 2:3]
ggplot(assoc_df, aes(y = assoc_df[, 1])) + 
  geom_point(aes(x = assoc_df[, 2]), 
             data = assoc_df, size = 3) + 
  theme_gdocs()

# bigram
tokenizer <- function(x) {
  NGramTokenizer(x, Weka_control(min = 2, max = 2))
}

malha_fina_bigram_tdm <- TermDocumentMatrix(malha_fina_clean_s, control = list(tokenize = tokenizer))
malha_fina_bigram_m <- as.matrix(malha_fina_bigram_tdm)
freq <- rowSums(malha_fina_bigram_m)
freq <- sort(freq, decreasing = TRUE)
bi_words <- names(freq)

# Criando paleta de cores para nuvem
paleta <- brewer.pal(10, "PuOr")
paleta <-paleta[-(1:2)]

wordcloud(bi_words, freq, max.words = 15)

png("wordcloud_bigram.png", width=12, height=8, units='in', res=300)
wordcloud(bi_words[2:length(bi_words)], freq, max.words = 30, colors = paleta)
dev.off()
