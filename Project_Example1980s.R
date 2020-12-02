library(genius)
library(tidyverse)
library(qdap)
library(tidytext)
data("stop_words")

# read in records of the year subset file
records <- read.csv("RecordYear_subset.csv", sep = ",", header=T)

# create data frame of lyrics from songs 1980s -  BE PATIENT!!!
lyrics <- records%>%
  add_genius(artist, track, type = "lyrics")

# create data frame of top 10 words
# occur at least 3 times
# remove common words using stop_word list in tidytext package
# plot 

# using freq_terms from qdap package
top10 <- freq_terms(lyrics$lyric, 10, at.least=3, 
                    stopwords=tidytext::stop_words$word)
plot(top10)

# using count from tidytext package

verse_words <- lyrics %>%
  unnest_tokens(word, lyric)

ft <- verse_words %>%
  anti_join(stop_words)

topten <- ft %>%
  count(word, sort = TRUE) %>%
  filter(n >= 3) %>%
  #filter(word != 'em') %>% #Review topten and filter words missed by stop_words
  top_n(10)

topten %>% 
  ggplot(aes(n, word)) +
  geom_col()+
  labs(y=NULL)

topyear <- ft %>% #NOTE that this data frame results in THREE variables not TWO!!
  group_by(year) %>%
  count(word, sort = TRUE) %>%
  filter(n >= 3) %>%
  #filter(word != 'em') %>% #Review topten and filter words missed by stop_words
  top_n(10)
