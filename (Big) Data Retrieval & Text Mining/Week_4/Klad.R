library(cleanNLP)
library(tidyverse)
library(lsa)
library(dplyr)


literatuur <- gutenbergr::gutenberg_download(c(10820, 19563, 29814), meta_fields = c("title", "author"))
stopwords_nl <- tibble(word = lsa::stopwords_nl)

original_books <- literatuur %>%
  group_by(title) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("[\\divxlc]+$",
                                           ignore_case = TRUE)))) %>%
  ungroup()


#naar tidy formaat
tidy_books <- original_books %>%
  tidytext::unnest_tokens(word, text)

#verwijder stop words
tidy_books <- tidy_books %>%
  anti_join(stopwords_nl)

#woorden tellen
tidy_books %>%
  count(word, sort = TRUE)

#wordcloud
tidy_books %>%
  count(word) %>%
  with(wordcloud::wordcloud(word, n, max.words = 50))

# veel nietszeggende woorden

cleanNLP::cnlp_init_udpipe("dutch")

book_vector_liefde <- original_books %>% filter(title == 'Een liefde') %>% select(text)
book_vector_kom <- original_books %>% filter(title == 'De komedianten') %>% select(text)
book_vector_eline <- original_books %>% filter(title == 'Eline Vere: Een Haagsche roman') %>% select(text)

corpus <- cleanNLP::cnlp_annotate(book_vector_liefde)
View(corpus$token)
corpus <- cleanNLP::cnlp_annotate(book_vector_kom)
View(corpus$token)
corpus <- cleanNLP::cnlp_annotate(book_vector_eline)
View(corpus$token)