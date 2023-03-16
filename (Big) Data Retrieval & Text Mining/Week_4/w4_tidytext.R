install.packages(c("janeaustenr","tidytext","wordcloud","gutenbergr","plotly","scales"))

library(tidyverse)
# Tidy text -------------------------------------------------------------------------------------------------------

##Jane Austen casus
#ruwe text uit package
original_books <- janeaustenr::austen_books()
#voeg regelnummers en hoofdstuknummers toe (per book)
original_books <- janeaustenr::austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]+$",
                                           ignore_case = TRUE)))) %>%
  ungroup()

#naar tidy formaat
tidy_books <- original_books %>%
  tidytext::unnest_tokens(word, text)

#verwijder stop words
tidy_books <- tidy_books %>%
  anti_join(tidytext::stop_words)

#woorden tellen
tidy_books %>%
  count(word, sort = TRUE)

#wordcloud
tidy_books %>%
  count(word) %>%
  with(wordcloud::wordcloud(word, n, max.words = 50))

#H.G. Wells boeken
options(gutenberg_mirror = "http://www.gutenberg.org")
hgwells <- gutenbergr::gutenberg_download(c(35, 36, 5230, 159),
                                         meta_fields = c("title"))
tidy_hgwells <- hgwells %>%
  tidytext::unnest_tokens(word, text) %>%
  rename(book = title) 

#Brönte boeken
bronte <- gutenbergr::gutenberg_download(c(1260, 768, 969, 9182, 767),
                                        meta_fields = c("title"))
tidy_bronte <- bronte %>%
  tidytext::unnest_tokens(word, text) %>%
  rename(book = title) 

#Combineer de drie data sets
frequency <- bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                      mutate(tidy_hgwells, author = "H.G. Wells"), 
                      mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  anti_join(tidytext::stop_words) %>% 
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = author, values_from = proportion)

#Beetje plotten
frequency_vs_austen <- frequency %>% pivot_longer(names_to = "author", values_to = "proportion", cols = `Bronte Sisters`:`H.G. Wells`)
ggplot(frequency_vs_austen, aes(x = proportion, y = `Jane Austen`, color = abs(`Jane Austen` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


#correlatie uitrekenen
cor.test(data = frequency_vs_austen[frequency_vs_austen$author == "Bronte Sisters",],
         ~ proportion + `Jane Austen`)
cor.test(data = frequency_vs_austen[frequency_vs_austen$author == "H.G. Wells",], 
         ~ proportion + `Jane Austen`)


# Frequenties per book ipv per auteur
frequency_books = bind_rows(mutate(tidy_bronte, author = "Bronte Sisters"),
                            mutate(tidy_hgwells, author = "H.G. Wells"), 
                            mutate(tidy_books, author = "Jane Austen")) %>% 
  mutate(word = str_extract(word, "[a-z']+"),
         book = str_c(author, " :: ", book)) %>%
  anti_join(tidytext::stop_words) %>% 
  count(book, word) %>%
  group_by(book) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  pivot_wider(names_from = book, values_from = proportion) 


# Plot m.b.v. Multi-Dimensional Scaling
# Bereken eerst de afstanden tussen de boeken in oorspronkelijke dimensies
matrix <- frequency_books %>% select(-word) %>% t() %>% dist()
# De matrix met afstanden gaat in cmdscale voor mult-dimensional scaling
# We voegen aan de dimensies nog de auteur en titel toe voor in de plot verderop
mds <- matrix %>%
  cmdscale() %>%
  as_tibble(.name_repair = "universal") %>%
  setNames(c("Dim1", "Dim2")) %>%
  mutate(label = colnames(frequency_books)[-1]) %>%
  separate(label, c("author","book"), " :: ")

plot <- ggplot(mds) + 
  geom_point(aes(x = Dim1, y = Dim2, shape = author, book = book))
# Plotly maakt van een ggplot een interactieve plot.
# De "unknown asthetics: book" komt in de popup-box terecht als je over een punt zweeft met
#   de muis en heeft dus wel degelijk een functie 
plotly::ggplotly(plot)


# Gebruik kmeans om te clusteren op de originele data, probeer verschillend aantal clusters
# set.seed is nodig omdat kmeans een heuristic is met een random element
set.seed(321) # probeer 321 en 1234 voor verschillende resultaten

# De kmeans functie berekent clusters uit een matrix met afstanden (zelfde input als MDS dus)
clust <- kmeans(matrix, 5)$cluster %>%
  as.factor()

mds <- mds %>%
  mutate(cluster = clust)
plot <- ggplot(mds) + 
  geom_point(aes(x = Dim1, y = Dim2, shape = author, book = book, color = cluster))
plotly::ggplotly(plot)



