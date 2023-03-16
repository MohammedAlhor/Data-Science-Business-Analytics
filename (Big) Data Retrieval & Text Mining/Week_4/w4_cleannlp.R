#install.packages(c("cleanNLP","data.tree","DiagrammeR"))

library(tidyverse)

# Natural Language Processing -------------------------------------------------------------------------------------

#Test teksten CleanNLP
text_en <- "Text mining, also referred to as text data mining, roughly equivalent to text analytics, is the process of deriving high-quality information from text. High-quality information is typically derived through the devising of patterns and trends through means such as statistical pattern learning. Text mining usually involves the process of structuring the input text (usually parsing, along with the addition of some derived linguistic features and the removal of others, and subsequent insertion into a database), deriving patterns within the structured data, and finally evaluation and interpretation of the output. 'High quality' in text mining usually refers to some combination of relevance, novelty, and interestingness. Typical text mining tasks include text categorization, text clustering, concept/entity extraction, production of granular taxonomies, sentiment analysis, document summarization, and entity relation modeling (i.e., learning relations between named entities)."
text_nl <- "Textmining, ook wel textdatamining, verwijst naar het proces om met allerhande ICT-technieken waardevolle informatie te halen uit grote hoeveelheden tekstmateriaal. Met deze technieken wordt gepoogd patronen en tendensen te ontwaren. Concreet gaat men teksten softwarematig structureren en ontleden, transformeren, vervolgens inbrengen in databanken, en ten slotte evalueren en interpreteren. Textmining is verwant aan tekstanalyse; de termen worden vaak door elkaar gebruikt."
slide_example <- 'Tom greets Jane and says "hello". Jane responds with a friendly nod.'



# Tokenization only -------------------------------------------------------

# Pure R (stringi) backend, alleen tokenization
cleanNLP::cnlp_init_stringi()
corpus <- cleanNLP::cnlp_annotate(text_en)

View(corpus$token)


# udpipe backend ----------------------------------------------------------

# Pure R (udpipe) backend, alle basis stappen plus dependency parsing
cleanNLP::cnlp_init_udpipe("dutch")
corpus <- cleanNLP::cnlp_annotate(text_nl)
View(corpus$token)

cleanNLP::cnlp_init_udpipe("english")
corpus <- cleanNLP::cnlp_annotate(slide_example)
View(corpus$token)


# Using dependencies ------------------------------------------------------

# Vind de parent token voor elke token in de tekst door te joinen op tid_source en tid
# Omdat tid een nummer is wat weer bij 0 begint bij een volgende zin moet je joinen op drie kolommen
corpus$token %>% 
  left_join(corpus$token, 
            by = c("tid_source" = "tid", "doc_id" = "doc_id", "sid" = "sid"), 
            suffix = c(".child", ".parent")) %>%
  View()

# We kunnen ook plot maken van de boomstructuur van een enkele zin
# Voor de conversie naar een data.tree moet de eerste kolom de eigen id zijn en de twee kolom de parent id
first_sentence <- corpus$token %>% 
  filter(doc_id == 1 & sid == 1) %>% 
  select(tid, tid_source, token, lemma, upos, relation, doc_id, sid)

# Boom structuren kunnen geplot worden door ze eerst naar een boomstructuur om te zetten
tree <- data.tree::FromDataFrameNetwork(first_sentence)
plot(tree)
# De namen van de nodes zijn standaard de id's wat niet heel prettig leesbaar is
# Deze namen kunnen we aanpassen (meer documentatie voor data.tree kan je vinden op https://cran.r-project.org/web/packages/data.tree/vignettes/data.tree.html)
tree$Do(function(node){node$name <- node$token})
plot(tree)


