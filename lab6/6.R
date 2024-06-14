library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("syuzhet")
library("ggplot2")
text <- readLines("europe.txt")
TextDoc <- Corpus(VectorSource(text))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
TextDoc <- tm_map(TextDoc, toSpace, "/")
TextDoc <- tm_map(TextDoc, toSpace, "@")
TextDoc <- tm_map(TextDoc, toSpace, "\\|")
TextDoc <- tm_map(TextDoc, toSpace, "ˆa“")
TextDoc <- tm_map(TextDoc, toSpace, ":")
TextDoc <- tm_map(TextDoc, toSpace, ";")
TextDoc <- tm_map(TextDoc, toSpace, ",")
TextDoc <- tm_map(TextDoc, toSpace, "ˆITM")
TextDoc <- tm_map(TextDoc, content_transformer(tolower))
# Remove numbers
TextDoc <- tm_map(TextDoc, removeNumbers)
# Remove english common stopwords
TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
# Remove your own stop word
# specify your custom stopwords as a character vector
TextDoc <- tm_map(TextDoc, removeWords, c("s", "company", "team"))
# Remove punctuations
TextDoc <- tm_map(TextDoc, removePunctuation)
# Eliminate extra white spaces
TextDoc <- tm_map(TextDoc, stripWhitespace)
# Text stemming - which reduces words to their root form
TextDoc <- tm_map(TextDoc, stemDocument)
# Text replacement
TextDoc <- tm_map(TextDoc, content_transformer(function(x) gsub(x, pattern = "mathemat", replacement = "math")))
TextDoc <- tm_map(TextDoc, content_transformer(function(x) gsub(x, pattern = " r ", replacement = " Rlanguage ")))
# Build a term-document matrix
TextDoc_dtm <- TermDocumentMatrix(TextDoc)
dtm_m <- as.matrix(TextDoc_dtm)
# Sort by descearing value of frequency
dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 5 most frequent words
head(dtm_d, 5)
barplot(dtm_d[1:20,]$freq, las = 2, names.arg = dtm_d[1:20,]$word,
        col ="lightgreen",
        main ="Top 20 most frequent words in the
        forms of knowledge\n assessment
        for the Big Data training courses",
        ylab = "Word frequencies")
#generate word cloud
set.seed(1234)
wordcloud(words = dtm_d$word, freq = dtm_d$freq, scale=c(5,0.5),
          min.freq = 1,
          max.words=100, random.order=FALSE,
          rot.per=0.40,
          colors=brewer.pal(8, "Dark2"))
# Find associations
#findAssocs(TextDoc_dtm, terms = c("system","busi","engin"), corlimit = 0.05)
findAssocs(TextDoc_dtm, terms = c("program","algorithm", "math", "statist"),
           corlimit = 0.5)
# Find associations for words that occur at least 30 times
findAssocs(TextDoc_dtm, terms = findFreqTerms(TextDoc_dtm, lowfreq = 30),
           corlimit = 0.5)
# regular sentiment score using get_sentiment() function and method of
# your choice
# please note that different methods may have different scales
syuzhet_vector <- get_sentiment(text, method="syuzhet")
# see the first row of the vector
head(syuzhet_vector)
# see summary statistics of the vector
summary(syuzhet_vector)
# bing
bing_vector <- get_sentiment(text, method="bing")
head(bing_vector)
summary(bing_vector)
#affin
afinn_vector <- get_sentiment(text, method="afinn")
head(afinn_vector)
summary(afinn_vector)
#compare the first row of each vector using sign function
rbind(
  sign(head(syuzhet_vector)),
  sign(head(bing_vector)),
  sign(head(afinn_vector))
)
d<-get_nrc_sentiment(as.vector(dtm_d$word))

head (d,10)
#transpose
td<-data.frame(t(d))
#The function rowSums computes column sums across rows for each level
# of a grouping variable.
td_new <- data.frame(rowSums(td[1:56]))
#Transformation and cleaning
names(td_new)[1] <- "count"
td_new <- cbind("sentiment" = rownames(td_new), td_new)
rownames(td_new) <- NULL
td_new2<-td_new[1:8,]
#Plot One - count of words associated with each sentiment
quickplot(sentiment, data=td_new2, weight=count, geom="bar", fill=sentiment,
          ylab="count")+ggtitle("Survey sentiments")
#Plot two - count of words associated with each sentiment, expressed
# as a percentage
barplot(
  sort(colSums(prop.table(d[, 1:8]))),
  horiz = TRUE,
  cex.names = 0.7,
  las = 1,
  main = "Emotions in Text", xlab="Percentage"
)

library("tidytext")
library("igraph")
library("ggraph")
library(dplyr)
text_df <- data_frame(line = 1, text = iconv(text,"cp1250","CP852"))
text_df
library(tidytext)
tidy_text <- text_df %>%
  unnest_tokens(word, text)
data(stop_words)
# we add some old words to stop_words
de <- data.frame("thy","OLD_WORDS")
names(de) <- c("word","lexicon")
stop_words <- rbind(stop_words,de)
de <- data.frame("1","OLD_WORDS")
names(de) <- c("word","lexicon")
de <- data.frame("hath","OLD_WORDS")
names(de) <- c("word","lexicon")
de <- data.frame("mar’d","OLD_WORDS")
names(de) <- c("word","lexicon")
stop_words <- rbind(stop_words,de)
#----------------------
tidy_text <- tidy_text %>%
  anti_join(stop_words)

tidy_text %>%
  count(word, sort = TRUE)
#----- Bigrams ---------------
text_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
text_bigrams
text_bigrams %>%
  count(bigram, sort = TRUE)
library(tidyr)
bigrams_separated <- text_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
# new bigram counts:
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
bigram_counts
bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")
bigrams_united
# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(word1 == "lord" | word2 == "lord") %>%
  graph_from_data_frame()
bigram_graph4 <- bigram_counts %>%
  filter(word1 == "marriage" | word2 == "marriage") %>%
  graph_from_data_frame()
bigram_graph5 <- bigram_counts %>%
  filter(word1 == "prince" | word2 == "prince") %>%
  graph_from_data_frame()

############plotting############################
dev.new()
ggraph(bigram_graph4, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                  end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), position = "identity") +
  theme_void()
dev.new(width = 550, height = 330, unit = "px")
ggraph(bigram_graph5, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                  end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), position = "identity") +
  theme_void()
#################################################################
# first level
bigram_graph1 <- bigram_counts %>%
  filter(word1 %in% c("marriage","prince") | word2 %in%
           c("marriage","prince"))
# second level
bigram_graph2 <- bigram_counts %>%
  filter(word1 %in% bigram_graph1$word1 | word1 %in%
           bigram_graph1$word2 | word2 %in% bigram_graph1$word1 |
           word2 %in% bigram_graph1$word2)
# end of second level
# third level
bigram_graph3 <- bigram_counts %>%
  filter(word1 %in% bigram_graph2$word1 | word1 %in%
           bigram_graph2$word2 | word2 %in% bigram_graph2$word1 |
           word2 %in% bigram_graph2$word2)
# end of third level
#################################################################
bigram_graph
set.seed(2016)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph1%>%graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                  end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
dev.new()
ggraph(bigram_graph2%>%graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                  end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
dev.new()
ggraph(bigram_graph3%>%graph_from_data_frame(), layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = TRUE,
                  end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +theme_void()

  