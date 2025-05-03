# cel projektu:
#stworzenie działającego skryptu w języku R, który analizuje dane tekstowe (z jednego
#lub wielu plików .txt, .csv) 
#przy pomocy algorytmów używanych podczas zajęć zidentyfikowanie i zbadanie trendów w danych tekstowych 
#(np. eksploracja częstości słów, analiza sentymentu, klastrowanie, modelowanie tematów itp.)

# WYMAGANIA TECHNICZNE PROJKTU

#Analiza danych tekstowych powinna wykorzystywać przynajmniej jedną z omawianych technik:
#np. analiza częstościsłów, analiza sentymentu, klastrowanie, itp.

#Użytkownik powinien mieć możliwość uruchomienia analizy na dostarczonych danych (plikach).

# W projekcie powinny znaleźć się elementy wizualizacji wyników (np. wykresy ggplot2, chmury słów).
# Kod musi być czytelnie udokumentowany (komentarze, podział na sekcje).

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

#???DO ZASTANOWIENIA CZY POTRZEBUJEMY WSZYSTKIE 
#Usuwanie znaków, symboli, wzorców 
corpus <- tm_map(corpus, toSpace, "[()\\[\\]]") # nawiasy kwadratowe i okrągłe
corpus <- tm_map(corpus, toSpace, "[@#$&*%+\\/=|]")
corpus <- tm_map(corpus, toSpace, "[\"'“”‘’„]") # cudzysłowy
corpus <- tm_map(corpus, toSpace, "[©]")
corpus <- tm_map(corpus, toSpace, "[✔✗]")
corpus <- tm_map(corpus, toSpace, "[ \t]{2,}") # tabulatory
corpus <- tm_map(corpus, toSpace, "\\S+\\.(pl|com)\\b")
corpus <- tm_map(corpus, toSpace, "http\\w*") #http i https



#CZYSZCZENIE DANYCH
corpus <- tm_map(corpus, content_transformer(tolower))#zamiana na małe litery
corpus <- tm_map(corpus, removeNumbers)#usunięcie liczb
#usunięcie stop-słów 
polish_stopwords <- c(
  "a", "aby", "ach", "acz", "aczkolwiek", "aj", "albo", "ale", "alez", "ależ", "ani",
  "az", "aż", "bardziej", "bardzo", "beda", "bedzie", "bez", "deda", "będą", "bede", "będę",
  "będzie", "bo", "bowiem", "by", "byc", "być", "byl", "byla", "byli", "bylo", "byly", "był",
  "była", "było", "były", "bynajmniej", "cala", "cali", "caly", "cała", "cały", "ci", "cie",
  "ciebie", "cię", "co", "cokolwiek", "cos", "coś", "czasami", "czasem", "czemu", "czy",
  "czyli", "daleko", "dla", "dlaczego", "dlatego", "do", "dobrze", "dokad", "dokąd", "dosc",
  "dość", "duzo", "dużo", "dwa", "dwaj", "dwie", "dwoje", "dzis", "dzisiaj", "dziś", "gdy",
  "gdyby", "gdyz", "gdyż", "gdzie", "gdziekolwiek", "gdzies", "gdzieś", "go", "i", "ich",
  "ile", "im", "inna", "inne", "inny", "innych", "iz", "iż", "ja", "jak", "jakas", "jakaś",
  "jakby", "jaki", "jakichs", "jakichś", "jakie", "jakis", "jakiś", "jakiz", "jakiż",
  "jakkolwiek", "jako", "jakos", "jakoś", "ją", "je", "jeden", "jedna", "jednak", "jednakze",
  "jednakże", "jedno", "jego", "jej", "jemu", "jesli", "jest", "jestem", "jeszcze", "jeśli",
  "jezeli", "jeżeli", "juz", "już", "kazdy", "każdy", "kiedy", "kilka", "kims", "kimś", "kto",
  "ktokolwiek", "ktora", "ktore", "ktorego", "ktorej", "ktory", "ktorych", "ktorym",
  "ktorzy", "ktos", "ktoś", "która", "które", "którego", "której", "który", "których",
  "którym", "którzy", "ku", "lat", "lecz", "lub", "ma", "mają", "mało", "mam", "mi", "miedzy",
  "między", "mimo", "mna", "mną", "mnie", "moga", "mogą", "moi", "moim", "moj", "moja",
  "moje", "moze", "mozliwe", "mozna", "może", "możliwe", "można", "mój", "mu", "musi", "my",
  "na", "nad", "nam", "nami", "nas", "nasi", "nasz", "nasza", "nasze", "naszego", "naszych",
  "natomiast", "natychmiast", "nawet", "nia", "nią", "nic", "nich", "nie", "niech", "niego",
  "niej", "niemu", "nigdy", "nim", "nimi", "niz", "niż", "no", "o", "obok", "od", "około",
  "on", "ona", "one", "oni", "ono", "oraz", "oto", "owszem", "pan", "pana", "pani", "po",
  "pod", "podczas", "pomimo", "ponad", "poniewaz", "ponieważ", "powinien", "powinna",
  "powinni", "powinno", "poza", "prawie", "przeciez", "przecież", "przed", "przede",
  "przedtem", "przez", "przy", "roku", "rowniez", "również", "sam", "sama", "są", "sie",
  "się", "skad", "skąd", "soba", "sobą", "sobie", "sposob", "sposób", "swoje", "ta", "tak",
  "taka", "taki", "takie", "takze", "także", "tam", "te", "tego", "tej", "ten", "teraz",
  "też", "to", "toba", "tobą", "tobie", "totez", "toteż", "totobą", "trzeba", "tu", "tutaj",
  "twoi", "twoim", "twoj", "twoja", "twoje", "twój", "twym", "ty", "tych", "tylko", "tym",
  "u", "w", "wam", "wami", "was", "wasz", "wasza", "wasze", "we", "według", "wiele", "wielu",
  "więc", "więcej", "wlasnie", "właśnie", "wszyscy", "wszystkich", "wszystkie", "wszystkim",
  "wszystko", "wtedy", "wy", "z", "za", "zaden", "zadna", "zadne", "zadnych", "zapewne",
  "zawsze", "ze", "zeby", "zeznowu", "zł", "znow", "znowu", "znów", "zostal", "został",
  "żaden", "żadna", "żadne", "żadnych", "że", "żeby"
)
corpus <- tm_map(corpus, removeWords, polish_stopwords)

corpus <- tm_map(corpus, removePunctuation)#usunięcie pozostałej interpunkcji
corpus <- tm_map(corpus, stripWhitespace)#usunięcie dodatkowych spacji

#Sprawdzanie
corpus[[1]]
                               
