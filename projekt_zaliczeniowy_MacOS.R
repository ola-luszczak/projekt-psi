
# Wymagane pakiety ----
library(tm)             
library(tidytext)      
library(SnowballC) 
library(topicmodels)   
library(dplyr)          
library(tidyverse) 
library(ggplot2)   
library(ggrepel) 
library(ggthemes)  
library(wordcloud)    
library(RColorBrewer) 
library(tcltk)  


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

# Funkcja pomocnicza: dzieli tekst na słowa, uzupełnia zredukowane formy (rdzenie) do pełnych słów
# na podstawie najdłuższego pasującego słowa w oryginalnym korpusie, a następnie skleja tekst z powrotem
complete_stems <- content_transformer(function(x, dict) {
  x <- unlist(strsplit(x, " "))                  
  x <- stemCompletion(x, dictionary = corpus_copy, type="longest") 
  paste(x, collapse = " ")                 
})


corpus_completed <- tm_map(corpus_stemmed, complete_stems, dict = corpus_copy)

#usuwanie NA
corpus_completed <- tm_map(corpus_completed, toSpace, "NA")
corpus_completed <- tm_map(corpus_completed, stripWhitespace)

#ponowne usunięcie stop słów
corpus_completed <- tm_map(corpus_completed, removeWords, custom_stopwords)
corpus_completed <- tm_map(corpus_completed, stripWhitespace)


#tokenizacja DTM
#tworzenie macierzy dokument-termin
dtm <- DocumentTermMatrix(corpus_completed)
dtm # informacje o macierzy
inspect(dtm)
dtm_m <- as.matrix(dtm)#konwertowanie macierzy dtm do zwykłej macierzy

dtm_m[1:5, 1:5]



#Chmury słów dla każdego dokumentu

for (i in 1:length(corpus)) {

  word_freq <- sort(dtm_m[i, ], decreasing = TRUE)
  word_df <- data.frame(word = names(word_freq), freq = word_freq)
  
  #zliczanie częstości słów
  v <- sort(dtm_m[i, ], decreasing = TRUE) #zliczanie częstości i sortowanie od najczęstrzych
  dtm_df <- data.frame(word = names(v), freq = v) #tworzenie ramki danych (word=słowa, freq= ich częstości)
  print(head(dtm_df, 10)) #wyświetlenie 10 najczęstrzych słów
  
  
  # Tworzenie chmury słów
  wordcloud(words = word_df$word, freq = word_df$freq, min.freq =4 , 
            colors = brewer.pal(9, "Spectral"), 
            main = paste("Chmura słów dla dokumentu", i))
  # Dodanie tytułu z nazwą pliku
  title(main = paste0("Chmura słów: ", basename(docs$filelist[i])))
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

#teraz neutralizujemy różnice wynikające z długości tekstów
total_words_bing <- tidy_dtm %>%
  group_by(document) %>%
  summarise(total_words = n())

sentiment_review_bing <- left_join(sentiment_review_bing, total_words_bing, by = "document") %>%
  mutate(score_avg = sentiment_score / total_words)

#nrc (przypisanie słów do kategorii ze względu na emocje - obliczamy procentowy udział emocji w dokumentach)
nrc_sentiment <- tidy_dtm %>%
  inner_join(get_sentiments("nrc"), by = c(term = "word"))

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

#teraz uśredniamy, żeby zneutralizować różnice w długości tekstów
total_words_afinn <- tidy_dtm %>%
  group_by(document) %>%
  summarise(total_words = n())

sentiment_review_afinn <- left_join(sentiment_review_afinn, total_words_afinn, by = "document") %>%
  mutate(score_avg = sentiment_score / total_words)


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

top_terms_by_topic_LDA <- function(dtm_input, 
                                   plot = TRUE, # domyślnie rysuje wykres
                                   k = number_of_topics) # wyznaczona liczba k tematów
{
# usuwanie wszystkich pustych wierszy z macierzy częstości
DTM <- dtm_input[unique(dtm_input$i), ]    # pobranie z DTM podzbioru tylko tych unikalnych indeksów 
  
# LDA - ukryta alokacja dirichleta
lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
topics <- tidy(lda, matrix = "beta") # słowa/tematy w uporządkowanym formacie tidy
  
# Wyodrębnienie dziesięciu słów o najwyższej wartości β (najbardziej charakterystycznych) dla każdego z tematów 
top_terms <- topics  %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) # uporządkowanie słów w malejącej kolejności informatywności
 

# Rysowanie wykresu dziesięiu najczęstszych słów dla każdego tematu

if (plot) {
  top_terms %>%
    mutate(term = reorder_within(term, beta,topic)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_x_reordered() +
    labs(x = "Terminy", y = "β (ważność słowa w temacie)", title = "Najbardziej informatywne słowa w tematach (LDA)") +
    coord_flip() +
    theme_minimal() +
    scale_fill_brewer(palette = "Spectral")
} else {
  return(top_terms)

}
}

# Dziesięć słów o największej informatywności według tematu

number_of_topics = 6
top_terms_by_topic_LDA(dtm)


