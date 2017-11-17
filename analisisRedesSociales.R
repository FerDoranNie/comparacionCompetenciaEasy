####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

library(magrittr) # magia magia pokemon
c("data.table", "dplyr", "ggplot2", "tidyr", "stringi", "stringr",
  "tm", "wordcloud", "ape", "wordcloud2", "lubridate", "tree", "rpart", 
  "rpart.plot") %>% 
  sapply(require, character.only=T)


# Globales ----------------------------------------------------------------
colores <- palettetown::pokepal("quilava", spread=12) 

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")


# Datos ----------------------------------------------------------------------
posteosFB <- read.csv("~/local/comparacionEasy_Uber_99/data/posteosFB.csv",
                      header = T)

comentariosFB <- read.csv("~/local/comparacionEasy_Uber_99/data/comentariosFB.csv", 
                          header = T)


tweets <-  read.csv("~/local/comparacionEasy_Uber_99/data/tweets.csv", 
                    header = T)


idsRedes <- read.csv("~/local/idsRedesSociales.csv", header = T)

# Manipulacion ------------------------------------------------------------
idsRedes <- idsRedes %>% 
  .[, c("id", "pais")]

posteosFB <- posteosFB %>% 
  separate(created_time, c("fecha", "hora"), remove=F, sep="T") %>% 
  data.table %>% 
  .[, fecha := as.Date(fecha)] %>% 
  .[, mes   := month(fecha)] %>% 
  .[, mesFactor   := factor(mes)] %>% 
  .[, dia   := day(fecha)] %>% 
  .[, anio  := year(fecha)] 

levels(posteosFB$mesFactor) <- meses[1:length(levels(posteosFB$mesFactor))]

comentariosFB <- comentariosFB %>% 
  separate(created_time, c("fecha", "hora"), remove=F, sep="T") %>% 
  data.table %>% 
  .[, fecha := as.Date(fecha)] %>% 
  .[, mes   := month(fecha)] %>% 
  .[, mesFactor   := factor(mes)] %>% 
  .[, dia   := day(fecha)] %>% 
  .[, anio  := year(fecha)] 

levels(comentariosFB$mesFactor) <- meses[1:length(levels(comentariosFB$mesFactor))]

tweets <- tweets %>% 
  separate(created, c("fecha", "hora"), remove=F, sep=" ") %>% 
  data.table %>% 
  .[, fecha := as.Date(fecha)] %>% 
  .[, mes   := month(fecha)] %>% 
  .[, mesFactor   := factor(mes)] %>% 
  .[, dia   := day(fecha)] %>% 
  .[, anio  := year(fecha)] 

levels(tweets$mesFactor) <- meses[1:length(levels(tweets$mesFactor))]

posteosFB <- merge(posteosFB, idsRedes, by.x="from_id", by.y="id") %>% 
  data.table %>% 
  .[, origen  := paste(from_name, pais, sep="_")]

origenPosteo <- posteosFB %>% 
  .[, c("id", "from_id", "origen")] %>% 
  rename(idPosteo = id) %>% 
  rename(idPagina = from_id)

comentariosFB <- merge(comentariosFB, origenPosteo, by="idPosteo") 

tweets <- merge(tweets, idsRedes, by.x="idOrigen", by.y="id") %>% 
  .[, origen  := paste(screenName, pais, sep="_")] 
  

# ANALISIS   ----------------------------------------------------------------------
### Facebook
# Promociones, campañas 
promociones <- posteosFB %>%  
  .[grepl("(?=.*promocion|promociones|regalo|regalos|promo)", message, perl=T, 
          ignore.case = T)]

descuento <- posteosFB %>%  
  .[grepl("(?=.*descuento|descuentos)", message, perl=T, ignore.case = T)]

  

# Quejas, felicitaciones, sugerencias
quejasFB <- comentariosFB %>% 
  .[grepl("(?=.*mal|servicio|pesimo|malisimo|queja|robo|mala experiencia|abuso|abusando|fraude|fraudulento|violencia|acoso|caro)", 
          message, perl=T, 
          ignore.case = T)] 

felicitacionesFB <- comentariosFB %>% 
  .[grepl("(?=.*excelente servicio|bueno|felicidades|me gusta|felicitaciones|agrado|felicito)", 
          message, perl=T, 
          ignore.case = T)] 


sugerenciasFB <- comentariosFB %>% 
  .[grepl("(?=.*sugerencia|sugerir|sugiero|recomiendo|recomendar|recomendaré|recomendare|pedir|opino|opinar|opinion)", 
          message, perl=T, 
          ignore.case = T)] 




# Vector_Sources ----------------------------------------------------------
origenes <- unique(comentariosFB$origen)

lapply(origenes, function(origen){
  X <- quejasFB %>% 
    .[origen==origen] %>% 
    .[, message := gsub("[[:punct:]]", "", message)] %>% 
    .[, "message"] %>%
    unlist %>%  
    iconv(to="ASCII//TRANSLIT") %>% 
    iconv(to="utf-8") %>% 
    VectorSource %>% 
    VCorpus() %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("sp")) %>% 
    tm_map(removeWords, stopwords("en")) %>% 
    tm_map(removeWords, stopwords("ge")) %>%
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(PlainTextDocument) %>% 
    tm_map(removeNumbers) 
    
  # x11()
  # wordcloud(X, max.words = 500, scale= c(8, 1),
  #             colors = colores[2:length(colores)], min.freq = 0.00001)
  # 
  # text(x = 0.5, y = 1, labels = origen)
  # 
  t1           <- TermDocumentMatrix(X)
  desparsedt1  <- removeSparseTerms(t1, 0.999)
  distribucion <- dist(scale(desparsedt1))
  cluster      <- hclust(distribucion)
  x11()
  # plot(cluster)
  plot.phylo(as.phylo(cluster), type="fan", font=2, cex = 1,
             edge.width = 2, tip.color = colores)
  text(x = 0.5, y = 2, labels = origen)
  
  
})






lapply(origenes, function(origen){
  X <- felicitacionesFB %>% 
    .[origen==origen] %>% 
    .[, message := gsub("[[:punct:]]", "", message)] %>% 
    .[, "message"] %>%
    unlist %>%  
    iconv(to="ASCII//TRANSLIT") %>% 
    iconv(to="utf-8") %>% 
    VectorSource %>% 
    VCorpus() %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("sp")) %>% 
    tm_map(removeWords, stopwords("en")) %>% 
    tm_map(removeWords, stopwords("ge")) %>%
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(PlainTextDocument) %>% 
    tm_map(removeNumbers) 
  
  x11()
  wordcloud(X, max.words = 500, scale= c(8, 1), 
            colors = colores[2:length(colores)], min.freq = 0.00001)
  
  text(x = 0.5, y = 1, labels = origen)
})

lapply(origenes, function(origen){
  X <- sugerenciasFB %>% 
    .[origen==origen] %>% 
    .[, message := gsub("[[:punct:]]", "", message)] %>% 
    .[, "message"] %>%
    unlist %>%  
    iconv(to="ASCII//TRANSLIT") %>% 
    iconv(to="utf-8") %>% 
    VectorSource %>% 
    VCorpus() %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("sp")) %>% 
    tm_map(removeWords, stopwords("en")) %>% 
    tm_map(removeWords, stopwords("ge")) %>%
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(PlainTextDocument) %>% 
    tm_map(removeNumbers) 
  
  x11()
  wordcloud(X, max.words = 500, scale= c(8, 1), 
            colors = colores[2:length(colores)], min.freq = 0.00001)
  
  text(x = 0.5, y = 1, labels = origen)
})


## Árboles de decisión
posteosFB %>% 
  .[angry_count==15]

lapply(origenes, function(x){
  # print(x)
  dato <- posteosFB %>% 
    .[origen==x]
  texto   <- dato$message %>%  unlist
  texto   <- gsub("http\\S+\\s*", "", texto)
  texto   <- iconv(texto, to="ASCII//TRANSLIT")
  Corpora <- VCorpus(VectorSource(texto))
  Corpora <- tm_map(Corpora, content_transformer(tolower))
  Corpora <- tm_map(Corpora, removeNumbers)
  Corpora <- tm_map(Corpora, removeWords, stopwords("spanish"))
  Corpora <- tm_map(Corpora, removeWords, stopwords("english"))
  Corpora <- tm_map(Corpora, removeWords, stopwords("po"))
  Corpora <- tm_map(Corpora, removePunctuation)
  Corpora <- tm_map(Corpora, stripWhitespace)
  Corpora <- tm_map(Corpora, PlainTextDocument)
  DTM     <- DocumentTermMatrix(Corpora)
  DTM     <- removeSparseTerms(DTM, 0.99)
  DTM     <- as.matrix(DTM)
  data_test <-  cbind(dato[,"likes_count"], DTM)

  ARBOL <- tree(likes_count ~ ., data = as.data.frame(data_test),
                control = tree.control(100000, mindev = 0.00002))
  
  ARBOLli<-rpart(likes_count ~ ., data=as.data.frame(data_test), 
                 control= rpart.control(minsplit=10,cp=0.006,xval=10))
  
  maximo <- data_test$likes_count %>%  max
  # print(maximo)
  palabra <- data_test %>%
    .[likes_count==maximo] %>%
    gather(tipo, valor, -likes_count) %>%
    .[which.max(.$valor), ] %>%
    .$tipo
  
  palabras <- data_test %>%
    .[likes_count==maximo] %>%
    gather(tipo, valor, -likes_count) 
  
  print(length(unique(palabras$tipo)))
  z <- c(palabra, maximo)
  print(ARBOLli)
  # print(sum(data_test$likes_count, na.rm=T))
  # x11()
  # plot(ARBOL, main=z)
  # text(ARBOL)
  # rpart.plot(ARBOLli, type = 2, extra= 1, box.palette = "Greens", cex = 0.8)
  # plot(ARBOL); text(ARBOL)
  
})


posteosFB %>% 
  .[origen=="Uber_brasil"] %>% 
  .[grepl("escolha", message, ignore.case = T)] %>% 
  head

texto   <- posteosFB$message %>%  unlist
texto   <- gsub("http\\S+\\s*", "", texto)
texto   <- iconv(texto, to="ASCII//TRANSLIT")
Corpora <- VCorpus(VectorSource(texto))
Corpora <- tm_map(Corpora, content_transformer(tolower))
Corpora <- tm_map(Corpora, removeNumbers)
Corpora <- tm_map(Corpora, removeWords, stopwords("spanish"))
Corpora <- tm_map(Corpora, removeWords, stopwords("english"))
Corpora <- tm_map(Corpora, removeWords, stopwords("po"))
Corpora <- tm_map(Corpora, removePunctuation)
Corpora <- tm_map(Corpora, stripWhitespace)
Corpora <- tm_map(Corpora, PlainTextDocument)      
DTM     <- DocumentTermMatrix(Corpora)
DTM     <- removeSparseTerms(DTM, 0.99)
DTM     <- as.matrix(DTM)

data_test <-  cbind(posteosFB[,"likes_count"], DTM)

data_test$likes_count %>%  max
data_test %>% 
  .[likes_count==541600] %>% 
  gather(tipo, valor, -likes_count) %>% 
  .[which.max(.$valor), ] %>% 
  .$tipo

data_test %>% head
ARBOL <- tree(likes_count ~ ., data = as.data.frame(data_test),
             control = tree.control(100000, mindev = 0.00002))

plot(ARBOL); text(ARBOL)

modelo_l <- glm(likes_count ~., family = quasipoisson(link=log),
               data=as.data.frame(data_test))

summary(modelo_l)
# anova(modelo_l,test="Chisq")

ARBOLli <-rpart(likes_count ~ ., data=as.data.frame(data_test), 
                control= rpart.control(minsplit=10,cp=0.006,xval=10))

rpart.plot(ARBOLli, type = 2, extra= 1, box.palette = "Greens", cex = 0.8)



### Twitter
promocionesTwitter <- tweets %>% 
  .[grepl("(?=.*promocion|promociones|regalo|regalos|promo)", text, perl=T, 
          ignore.case = T)]

descuentoTwitter <- tweets %>% 
  .[grepl("(?=.*descuento|descuentos)", text, perl=T, ignore.case = T)]

