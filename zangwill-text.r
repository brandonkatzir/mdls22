install.packages("dplyr")
install.packages("tm.plugin.webmining")
install.packages("purrr")
install.packages("tidytext")
install.packages("gutenbergr")
install.packages("ggplot2")
install.packages("stringr")
install.packages("igraph")
install.packages("ggraph")
######
library(dplyr)
library(tm.plugin.webmining)
library(purrr)
library(tidytext)
library(gutenbergr)
library(ggplot2)
library(stingr)
library(igraph)
library(ggraph)
####
zangwill <- gutenberg_download(c(35076, 29875, 12680, 28982, 35238, 38413))
###
tidy_zangwill <- zangwill %>%
  unnest_tokens(word, text)

#as.data.frame(zangwill)
####
tidy_zangwill %>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) %>%
  filter(n>600) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(n, word)) +
 geom_col() +
  labs(y=NULL)
###
zangwill_bigrams <- zangwill %>%
  unnest_tokens(bigram, text, token="ngrams", n=2)
zangwill_bigrams

zangwill_bigrams %>%
  count(bigram, sort=TRUE)
#the following code removes stop words from bigrams ADD TINY R BEFORE COMPLETING THIS STEP
bigrams_separated <- zangwill_bigrams %>%
  separate(bigram, c("word1", "word2"), sep=" ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 
#new bigram counts omitting common words
bigrams_counts <- bigrams_filtered %>%
  count(word1, word2, sort=TRUE)
bigrams_counts

zangwill_trigrams <- zangwill %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

zangwill_trigrams

bigrams_filtered %>%
  filter(word2=="eyes") %>%
  count(word1, sort=TRUE)




bigram_graph <- bigrams_counts %>%
  filter(n>20) %>%
  graph_from_data_frame()


set.seed(2017)
ggraph(bigram_graph, layout= "fr") + 
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), vjust=1, hjust=1)
#everything above this line works