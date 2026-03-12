---
title: "Zajecia 3"
author: "Piotr Krzeminski"
date: "12/03/2026"
output: html_document
---

```{r setup, include=FALSE}
# Ukrywamy komunikaty ładowania pakietów, ale zostawiamy widoczny kod (echo = TRUE)
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
```

## Instalacja i ładowanie wymaganych pakietów

```{r init}
library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)

# Stworzenie funkcji do przetwarzania tekstu
process_text <- function(file_path) {
  text <- tolower(readLines(file_path, encoding = "UTF-8"))
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  text <- removeWords(text, stopwords("en"))
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[words != ""]
  return(words)
}

# Stworzenie funkcji do obliczania częstości występowania słów
word_frequency <- function(words) {
  freq <- table(words)
  freq_df <- data.frame(word = names(freq), freq = as.numeric(freq))
  freq_df <- freq_df[order(-freq_df$freq), ]
  return(freq_df)
}

# Stworzenie funkcji do tworzenia chmury słów
plot_wordcloud <- function(freq_df, title, color_palette = "Dark2") {
  wordcloud(words = freq_df$word, freq = freq_df$freq, min.freq = 16,
            colors = brewer.pal(8, color_palette))
  title(title)
}
```

## Analiza pojedynczego pliku (Biden 2021)

Wczytanie i przetworzenie tekstu z pierwszego przemówienia.

```{r single_file}
file_path <- "Biden2021.txt"
words <- process_text(file_path)

# Obliczenie częstości występowania słów
freq_df <- word_frequency(words)

# Tworzenie chmury słów
plot_wordcloud(freq_df, "Chmura słów - 2021", "Dark2")

# Wyświetlenie 10 najczęściej występujących słów
print(head(freq_df, 10))
```

### Usunięcie dodatkowych stop słów

Usuwamy dodatkowe znaki i skróty, które zaburzają analizę (np. pauzy i apostrofy).

```{r single_file_clean}
custom_stopwords <- c("—", "–", "’s", "’re", "’ve", "’m")

# Usunięcie dodatkowych stop słów z przetworzonego tekstu 
words <- words[!words %in% custom_stopwords]

freq_df <- word_frequency(words)
plot_wordcloud(freq_df, "Chmura słów po czyszczeniu", "Dark2")
print(head(freq_df, 10))
```

## Analiza dwóch plików równocześnie

Porównanie przemówień z lat 2021 oraz 2024 przed czyszczeniem niestandardowych stop słów.

```{r multiple_files}
# Lista plików do wczytania
file_paths <- c("Biden2021.txt", "Biden2024.txt") 

# Przetwarzanie każdego pliku osobno
for (file_path in file_paths) {
  words <- process_text(file_path)
  freq_df <- word_frequency(words)
  
  plot_wordcloud(freq_df, paste("Chmura słów -", file_path), "Dark2")
  
  cat("Najczęściej występujące słowa w pliku", file_path, ":\n")
  print(head(freq_df, 10))
  cat("\n")
}
```

### Analiza dwóch plików po dodatkowym czyszczeniu

```{r multiple_files_clean}
for (file_path in file_paths) {
  words <- process_text(file_path)
  words <- words[!words %in% custom_stopwords]
  freq_df <- word_frequency(words)
  
  plot_wordcloud(freq_df, paste("Chmura słów (czyste) -", file_path), "Dark2")
  
  cat("Najczęściej występujące słowa w pliku", file_path, ":\n")
  print(head(freq_df, 10))
  cat("\n")
}
```
