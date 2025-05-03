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

#Wymagane biblioteki
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
toSpace <- content_transformer(function (x, patern) gsub(pattern, t" ", x))

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
corpus <- tm_map(corpus, removeWords, stopwords("pl"))#usunięcie stop-słów
corpus <- tm_map(corpus, removePunctuation)#usunięcie pozostałej interpunkcji
corpus <- tm_map(corpus, stripWhitespace)#usunięcie dodatkowych spacji

#Sprawdzanie
corpus[1][1]
