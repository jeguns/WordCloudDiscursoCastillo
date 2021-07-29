
# =========================== #
# Introducción al Text Mining #
# =========================== #

# Mg. Jesús Eduardo Gamboa Unsihuay
# Docente auxiliar - Depto. Académico de Estadística e Informática
# jgamboa@lamolina.edu.pe

# -------------------- #
# 1. Carga de paquetes #
# -------------------- #

library(stringi)
library(tm)
library(dplyr)
library(caret)
library(tibble)
library(tidyverse)
library(tidytext)
library(purrr)
library(pdftools)


limpieza = function(texto,idioma,elim){
  texto = stri_trans_general(str = texto, id = "Latin-ASCII")
  texto = gsub("http[^[:blank:]]+","",texto) # elimna http
  texto = gsub("@","",texto) # elimina @
  texto = gsub("\n|\t","",texto) # elimina saltos de línea y tabulaciones
  texto = gsub("[[:punct:]]|[[:digit:]]"," ",texto) # elimina puntuación y caracteres numéricos 
  texto = gsub("\\b\\w{1}\\b"," ",texto) # elmina palabras de una letra
  texto = tolower(texto)
  texto = removePunctuation(texto)
  texto = removeWords(texto, words = elim)
  texto = stripWhitespace(texto)
  return(texto)
}

files       = list.files(pattern = "pdf")
files # lee la lista de archivos pdf en la carpeta
PDFS        = lapply(files, pdf_text) # pdftools::pdf_text - lectura alfabética
eliminar    = c(stopwords("spanish")[-162],'u','f','✓','✓ ')
documentos  = map(PDFS, limpieza, "spanish", eliminar) 
corpus      = Corpus(VectorSource(documentos)) 
mtd         = corpus %>% 
  tm_map(removePunctuation) %>% 
  DocumentTermMatrix() %>% 
  removeSparseTerms(sparse = 0.6)
mtd
inspect(mtd)

DATOS = data.frame(palabra = rownames(as.data.frame(t(as.matrix(mtd)))),
                   frecuencia = t(as.matrix(mtd))[,1]) %>% 
  dplyr::filter(palabra != "u2713")
 
                       
library(wordcloud)
x11();par(mar=c(0.5,0.5, 1, 0.5))
wordcloud(words = DATOS[,1], 
          freq  = DATOS[,2],
          min.freq = 3, 
          max.words = Inf, 
          colors = brewer.pal(n = 8, name = 'Dark2'),
          scale=c(3,.2))
text(x=0.5, y=0.975, " Términos más empleados en el primer discurso del Presidente Pedro Castillo")
