
# Wymagane pakiety ----
library(tm)             
library(tidytext)      
library(SnowballC) 
library(SentimentAnalysis) 
library(topicmodels)   
library(dplyr)          
library(stringr)
library(tidyverse) 
library(DT)
library(ggplot2)   
library(ggrepel) 
library(ggthemes)  
library(wordcloud)    
library(RColorBrewer) 
library(cluster)       
library(factoextra) 
library(tcltk)  
library(textstem)
library(scales)

#WCZYTANIE TEKSTU, USUNIĘCIE ZBĘDNYCH ZNAKÓW, CZYSZCZENIE DANYCH

folder_path <- tk_choose.dir()

#Wczytywanie wszystkich plików z wybranego folderu
docs <- DirSource(folder_path)

#tworzenie korpusu plików
corpus <- VCorpus(docs)

#zapewnienie kodowania w całym korpusie
corpus <- tm_map(corpus, content_transformer(function(x) iconv(x, to = "UTF-8", sub = "byte")))

#funkcja, która będzie zamieniać zbędne znaki na spacje
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))


#Usuwanie znaków, symboli, wzorców 
corpus <- tm_map(corpus, toSpace, "[()\\[\\]]") # nawiasy kwadratowe i okrągłe
corpus <- tm_map(corpus, toSpace, "[@#$&*%+\\/=|—-]")
corpus <- tm_map(corpus, toSpace, "[\"'“”‘’„]") # cudzysłowy,apostrof
corpus <- tm_map(corpus, toSpace, "[©]")
corpus <- tm_map(corpus, toSpace, "[✔✗]")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}") # tabulatory
corpus <- tm_map(corpus, toSpace, "\\S+\\.(pl|com)\\b")#źródła o domenach .com .pl
corpus <- tm_map(corpus, toSpace, "http\\w*") #http i https



#CZYSZCZENIE DANYCH
corpus <- tm_map(corpus, content_transformer(tolower))#zamiana na małe litery
corpus <- tm_map(corpus, removeNumbers)#usunięcie liczb

#usunięcie stop-słów 
#stop słowa z pakietu tidytext
custom_stopwords <- stop_words$word
corpus <- tm_map(corpus, removeWords, custom_stopwords)


corpus <- tm_map(corpus, removePunctuation)#usunięcie pozostałej interpunkcji
corpus <- tm_map(corpus, stripWhitespace)#usunięcie dodatkowych spacji

#Sprawdzanie
corpus[[1]]

#STEMMING

corpus_copy <- corpus #korpus oryginalny

corpus_stemmed <- tm_map(corpus, stemDocument)#korpus po stemmingu

corpus_stemmed[[1]]

#Stem completion

#funkcja pomocnicza, dzieli tekst na słowa, uzupełnia rdzenie do pełnych wyrazów (do najdłuższego słowa o danym rdzeniu z tekstu bazowego), łączy z powrotem w tekst
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") 
  paste(x, collapse = " ")                 
})


corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

#usówanie NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)

#ponowne usunięcie stop słów
corpus_completed <- tm_map(corpus_completed, removeWords, custom_stopwords)
corpus_completed <- tm_map(corpus_completed, stripWhitespace)


#tokenizacja DTM
dtm <- DocumentTermMatrix(corpus_completed)
dtm # informacje o macierzy
inspect(dtm)
dtm_m <- as.matrix(dtm)#konwertowanie macierzy dm do zwykłej macierzy

dtm_m[1:5, 1:5]



#Chmury słów dla każdego dokumentu

for (i in 1:length(corpus)) {

  dtm_i <- DocumentTermMatrix(VCorpus(VectorSource(corpus[[i]]$content)))#tokenizacja jeszcze raz oddzielnie dla każdego dokumentu
  dtm_m_i <- as.matrix(dtm_i)
  word_freq <- sort(colSums(dtm_m_i), decreasing = TRUE)
  word_df <- data.frame(word = names(word_freq), freq = word_freq)
  
  #zliczanie częstości słów
  v <- sort(colSums(dtm_m_i), decreasing = TRUE) #zliczanie częstości i sortowanie od najczęstrzych
  dtm_df <- data.frame(word = names(v), freq = v) #tworzenie ramki danych (word=słowa, freq= ich częstości)
  print(head(dtm_df, 10)) #wyświetlenie 10 najczęstrzych słów
  
  
  # Tworzenie chmury słów
  wordcloud(words = word_df$word, freq = word_df$freq, min.freq =4 , 
            colors = brewer.pal(9, "Spectral"), 
            main = paste("Chmura słów dla dokumentu", i))
}

#ANALIZA SENTYMENTU

#wczytanie słowników bing i nrc 
bing_path <- tk_choose.files()
bing <- read_csv(bing_path)

nrc_path <- tk_choose.files()
nrc <- read_csv(nrc_path)

afinn_path <- tk_choose.files()
afinn <- read_csv(afinn_path)


tidy_dtm<-tidy(dtm)

#bing (positive/negative)

bing_sentiment <- tidy_dtm %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

sentiment_review_bing <- bing_sentiment %>%
  count(document, sentiment) %>% #zliczanie ile pozytywnych i negatywnych słów w każdym dokumencie
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%  #zmiana danych tak, żeby mieć osobno kolumny dla słów pozytywnych i negatywnych w każdym dokumencie żeby łatwiej porównać czy tekst był bardziej pozytywny czy negatywny
  mutate(sentiment_score = positive - negative) #nowa koluma która przedstawia ogólny wynik nastroju dokumentu


#nrc (przypisanie do kategorii - przeliczamy procentowy udział emocji w tekstach)
nrc_sentiment <- tidy_dtm %>%
  inner_join(get_sentiments("nrc"), by = c(term = "word"),relationship = "many-to-many")

nrc_sentiment_percent <- nrc_sentiment %>%
  count(document, sentiment) %>%
  group_by(document) %>%
  mutate(percent = n / sum(n)) %>% #udział słów z danej kategorii w tekście
  ungroup()


#afinn (punktowanie sentymentu, wartości ujemne i dodatnie)
afinn_sentiment <- tidy_dtm %>%
  inner_join(get_sentiments("afinn"), by = c(term = "word"))

sentiment_review_afinn <- afinn_sentiment %>%
  group_by(document) %>%
  summarise(sentiment_score = sum(value))



#WYKRESY

#bing-wykres(który przedstawi łączny wynik sentymentu dla każdego dokumentu osobno)
ggplot(sentiment_review_bing, aes(x = document, y = sentiment_score, fill = document)) +
  geom_col(width = 0.5) +
  theme_minimal() +
  labs(title = "Łączny sentyment w dokumencie (BING)",
       x = "Dokument", y = "Łączny sentyment")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#wykresy nrc
#wykresy oddzielne
ggplot(nrc_sentiment_percent, aes(x = document, y = percent, fill = document)) +
  geom_col(width = 0.5) +
  facet_wrap(~ sentiment, scales = "free_y") +  # ~ wymusza utworzenie utworzenie osobnego wykresu dla każdej wartości sentymentu
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal() +
  labs(
    title = "Procentowy udział konkretnych emocji w dokumentach (NRC)",
    x = "Dokument",
    y = "Udział emocji (%)"
  ) +
  theme(
    strip.text = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )
#wykres zbiorczy
ggplot(nrc_sentiment_percent, aes(x = document, y = percent, fill = sentiment)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Procentowy udział emocji w wypowiedziach polityków (NRC)",
    x = "Polityk",
    y = "Udział emocji (%)",
    fill = "Emocja"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#wykres afinn
ggplot(sentiment_review_afinn, aes(x = document, y = sentiment_score, fill = document)) +
  geom_col(width = 0.5) +
  theme_minimal() +
  labs(title = "Łączny sentyment w dokumencie (AFINN)", x = "Dokument", y = "Łączny sentyment")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")




#MODELOWANIE TEMATÓW
#funkcja top_terms_by_topis_LDA - wczytanie tekstu (z wektora, kolumny tekstowej, lub z ramki danych), wizualizacja słów o najdiększej informatywności przy użyciu metody LDA, dla wyznaczonej liczby tematów

top_terms_by_topic_LDA <- function(input_text, # wektor lub kolumna tekstowa z ramki danych
                                   plot = TRUE, # domyślnie rysuje wykres
                                   k = number_of_topics) # wyznaczona liczba k tematów
{
# usuwanie wszystkich pustych wierszy z macierzy częstości
unique_indexes <- unique(dtm$i) # pobranie indeksów unikalnych wartości
DTM <- dtm[unique_indexes,]    # pobranie z DTM podzbioru tylko tych unikalnych indeksów 
  
# LDA - ukryta alokacja dirichleta
lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
topics <- tidy(lda, matrix = "beta") # słowa/tematy w uporządkowanym formacie tidy
  
# dziesięć najczęstszych słów dla każdego tematu
top_terms <- topics  %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) # uporządkowanie słów w malejącej kolejności informatywności
 

# Rysowanie wykresu dziesięiu najczęstszych słów dla każdego tematu

if (plot) {
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    labs(x = "Terminy", y = "β (ważność słowa w temacie)") +
    coord_flip() +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral")
} else {
  return(top_terms)

}
}

# Dziesięć słów o największej informatywności według tematu

number_of_topics = 6
top_terms_by_topic_LDA(dtm_df$word)


