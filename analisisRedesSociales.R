####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

library(magrittr) # magia magia pokemon
c("data.table", "ggplot2", "stringi", "stringr",
  "tm", "wordcloud", "ape", "wordcloud2", "lubridate", "tree", "rpart", 
  "rpart.plot", "igraph") %>% 
  sapply(require, character.only=T)


# Globales ----------------------------------------------------------------
colores <- palettetown::pokepal("quilava", spread=12) 

meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
           "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

risaIndeseable <- c("ja", "je", "ji", "jo")

risaIndeseable <- lapply(risaIndeseable, function(x){
  z <- mapply(rep, x, times=1:10)
  z <- sapply(z, paste, collapse="")
  z <- z %>%  unlist
})

risaIndeseable <- risaIndeseable  %>%  unlist %>% 
  paste(sep=",")

indeseables <- c("buenos", "dias", "tardes", "noches", "mas", "gracias", "hola", 
                 "noche", "dia", "tarde", "nao", "sao")

# indeseables <- c(indeseables, risaIndeseable)


# Datos ----------------------------------------------------------------------
posteosFB <- read.csv("~/local/comparacionEasy_Uber_99/data/posteosFB.csv",
                      header = T, stringsAsFactors = F)

comentariosFB <- read.csv("~/local/comparacionEasy_Uber_99/data/comentariosFB.csv", 
                          header = T, stringsAsFactors = F)


tweets <-  read.csv("~/local/comparacionEasy_Uber_99/data/tweets.csv", 
                    header = T)


idsRedes <- read.csv("~/local/idsRedesSociales.csv", header = T) %>% 
  data.table


palabras <- read.csv("~/local/palabrasEspaniol/SpanishDAL-v1.2/meanAndStdev.csv",
                    header = F, sep = ";")

names(palabras) <- c("palabra", "media_agrado", "media_activacion",
                     "media_imaginabilidad",
                     "stdev_agrado", "stdev_activacion", "stdev_imaginabilidad")

separados <- do.call("rbind", 
                     strsplit(as.character(palabras$palabra), "_", fixed = T)) %>% 
  data.frame()

palabras        <- cbind(separados, palabras[-1]) 
names(palabras) <- c("palabra","tipo", "media_agrado", "media_activacion", 
                     "media_imaginabilidad","stdev_agrado", "stdev_activacion", 
                     "stdev_imaginabilidad")



palabras <- palabras %>%  
  data.table %>% 
  .[, sentimiento := ifelse(media_agrado<2, 
                            "Negativo", 
                            ifelse(media_agrado>2, "Positivo","Neutro"))] %>%  
  .[, tipo_palabra := ifelse(tipo=="N", "Sustantivo", 
                             ifelse(tipo=="V","Verbo",
                                    ifelse(tipo=="A",
                                           "Adjetivo","Adverbio")))] %>% 
  .[, sentimientoAgrado := ifelse(sentimiento=="Positivo",1, 
                                  ifelse(sentimiento=="Negativo", -1, 0))]



palabrasEntrenamiento <- palabras %>% 
  .[, c("palabra", "sentimiento")]


# Manipulacion ------------------------------------------------------------
idsFb <- idsRedes %>% 
  .[red=="facebook"] %>% 
  .$id

idsTw <- idsRedes %>% 
  .[red=="twitter"] %>% 
  .$id

idsRedes <- idsRedes %>% 
  .[, c("id", "pais")]

posteosFB <- posteosFB %>% 
  tidyr::separate(created_time, c("fecha", "hora"), remove=F, sep="T") %>% 
  data.table %>% 
  .[, fecha := as.Date(fecha)] %>% 
  .[, mes   := month(fecha)] %>% 
  .[, mesFactor   := factor(mes)] %>% 
  .[, dia   := day(fecha)] %>% 
  .[, anio  := year(fecha)] 

levels(posteosFB$mesFactor) <- meses[1:length(levels(posteosFB$mesFactor))]

comentariosFB <- comentariosFB %>% 
  tidyr::separate(created_time, c("fecha", "hora"), remove=F, sep="T") %>% 
  data.table %>% 
  .[, fecha := as.Date(fecha)] %>% 
  .[, mes   := month(fecha)] %>% 
  .[, mesFactor   := factor(mes)] %>% 
  .[, dia   := day(fecha)] %>% 
  .[, anio  := year(fecha)] 

levels(comentariosFB$mesFactor) <- meses[1:length(levels(comentariosFB$mesFactor))]

tweets <- tweets %>% 
  tidyr::separate(created, c("fecha", "hora"), remove=F, sep=" ") %>% 
  data.table %>% 
  .[, fecha := as.Date(fecha)] %>% 
  .[, mes   := month(fecha)] %>% 
  .[, mesFactor   := factor(mes)] %>% 
  .[, dia   := day(fecha)] %>% 
  .[, anio  := year(fecha)] 

levels(tweets$mesFactor) <- meses[1:length(levels(tweets$mesFactor))]

posteosFB <- merge(posteosFB, idsRedes, by.x="from_id", by.y="id") %>% 
  data.table %>% 
  .[, origen  := paste(from_name, pais, sep="_")] %>% 
  .[, origen  := gsub("[[:space:]]", "_", origen)]


origenPosteo <- posteosFB %>% 
  .[, c("id", "from_id", "origen")] %>% 
  dplyr::rename(idPosteo = id) %>% 
  dplyr::rename(idPagina = from_id)

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
comentariosFB <- comentariosFB %>% 
  .[!from_id %in% idsFb]

palabrasFelicidad <- c("excelente","bueno", "felicidades", "me gusta", 
                       "felicitaciones", "agrado", "felicito", "parabens", 
                       "parabéns", "bom", "gusta")

palabrasQuejas <- c("mal", "pesimo", "malisimo", "queja", "robo",
                    "mala", "abuso", "abusando", "deficiente", "fraude", 
                    "fraudulento", "violencia", "acoso", "caro", "ruim", "pobre",
                    "horrible")

palabrasAyuda <- c("ayuda", "ayudenme", "ajuda", "help", "solicito ayuda", 
                   "Peço ajuda", "auxilio")

palabrasRecomendacion <- c("sugerencia", "sugerir", "sugiero", "recomiendo", 
                           "recomendar", "recomendaré", "recomendare", "pedir", 
                           "opino", "opinar", "opinion")

palabrasFelicidad     <- paste(palabrasFelicidad, collapse="|")
palabrasQuejas        <- paste(palabrasQuejas, collapse="|")
palabrasAyuda         <- paste(palabrasAyuda, collapse="|")
palabrasRecomendacion <- paste(palabrasRecomendacion, collapse="|")

felicitacionesFB <- comentariosFB %>% 
  .[grepl(paste("(", "?=.*", palabrasFelicidad, ")", sep=""), 
          message, perl=T, ignore.case = T)] %>% 
  .[!grepl(paste("(", "?=.*", palabrasQuejas, "|", 
                 palabrasAyuda, "|",palabrasRecomendacion, 
                 ")", sep=""), 
          message, perl=T, ignore.case = T)]

quejasFB <- comentariosFB %>% 
  .[grepl(paste("(", "?=.*", palabrasQuejas, ")", sep=""), 
          message, perl=T,  ignore.case = T)] %>% 
  .[!grepl(paste("(", "?=.*", palabrasFelicidad,
                 "|", palabrasAyuda, "|", 
                 palabrasRecomendacion,
                 ")", sep=""), 
          message, perl=T,  ignore.case = T)]


ayudaFB <- comentariosFB %>% 
  .[grepl(paste("(", "?=.*", palabrasAyuda, ")", sep=""), 
          message, perl=T, ignore.case = T)] %>%  
  .[!grepl(paste("(", "?=.*", palabrasQuejas,
                 "|", palabrasFelicidad, 
                 "|", palabrasRecomendacion,
                 ")", sep=""), 
        message, perl=T,  ignore.case = T)] 


sugerenciasFB <- comentariosFB %>% 
  .[grepl(paste("(", "?=.*", palabrasRecomendacion, ")", sep=""), 
          message, perl=T, ignore.case = T)] %>% 
  .[!grepl(paste("(", "?=.*", palabrasQuejas,
                 "|", palabrasFelicidad,
                 "|", palabrasAyuda,
                 ")", sep=""), 
          message, perl=T,  ignore.case = T)] 

# Vector_Sources ----------------------------------------------------------
origenes <- unique(comentariosFB$origen)

tmMaker <- function(datos){
  require(tm)
  x <- datos %>% 
    unlist 

  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("[[:digit:]]", "", x)
  
  x <- x %>%
    as.character %>%
    iconv(to="ASCII//TRANSLIT") %>%
    stri_enc_toutf8 %>% 
    # stri_escape_unicode %>% 
    # iconv(to="utf-8") %>%
    na.omit %>% 
    as.character %>% 
    VectorSource %>% 
    VCorpus %>%
    # inspect
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeWords, stopwords("sp")) %>% 
    tm_map(removeWords, stopwords("en")) %>%
    tm_map(removeWords, stopwords("ge")) %>%
    tm_map(removeWords, stopwords("po")) %>%
    # tm_map(removeWords, indeseables) %>%
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(PlainTextDocument) %>% 
    tm_map(removeNumbers) 
  
  return(x)
}

tmSentiment <- function(x){
  require(tm)
  X <- tmMaker(x)
  documentoMatrix <- TermDocumentMatrix(X)
  documentoMatrix <- slam::row_sums(documentoMatrix)
  palabras <- names(documentoMatrix)
  numeros  <-  as.numeric(documentoMatrix)
  documentoMatrix <- data.frame(palabra = palabras, numero  = numeros)
  documentoMatrix <- documentoMatrix %>% 
    data.table %>% 
    .[, palabra  := as.character(palabra)]
  
  documentoMatrix <- documentoMatrix %>% 
    dplyr::left_join(palabrasEntrenamiento, by="palabra") %>% 
    data.table %>% 
    .[, sentimiento  := ifelse(is.na(sentimiento), "Sin_clasificar", 
                               sentimiento)] %>% 
    .[, id := dplyr::row_number(sentimiento)] %>% 
    tidyr::spread(sentimiento, numero)
  
  # documentoMatrix[is.na(documentoMatrix)] <- 0
  # nombresmatrix   <- documentoMatrix[, 1]
  # documentoMatrix <- documentoMatrix %>% 
  #   dplyr::select(-palabra, -id) %>% 
  #   as.matrix
  # 
  # rownames(documentoMatrix)<- nombresmatrix
  
  return(head(documentoMatrix))
}

### Nubes de palabras

test <- felicitacionesFB %>%
  .[origen == origenes[3]] %>%
  .[, message := as.character(message)] %>%
  .[, message := gsub("[[:punct:]]", "", message)] %>%
  .[, c("message")]


tmSentiment(test)
# 
# 
# test %>%  
#   # unlist %>% 
#   # iconv(to="latin1") %>%
#   iconv(to="ASCII//TRANSLIT") %>%
#   stri_enc_toutf8 %>% 
#   # stri_escape_unicode %>% 
#   # iconv(to="utf-8") %>%
#   na.omit %>% 
#   as.character %>% 
#   VectorSource %>% 
#   VCorpus %>%
#   # inspect
#   tm_map(content_transformer(tolower)) %>% 
#   tm_map(removeWords, stopwords("sp")) %>% 
#   tm_map(removeWords, stopwords("en")) %>%
#   tm_map(removeWords, stopwords("ge")) %>%
#   tm_map(removeWords, stopwords("po")) %>%
#   # tm_map(removeWords, indeseables) %>%
#   tm_map(removePunctuation) %>% 
#   tm_map(stripWhitespace) %>% 
#   tm_map(PlainTextDocument) %>% 
#   tm_map(removeNumbers) %>% 
#     wordcloud
#   
# 
# tmMaker(test)


lapply(origenes, function(origin){
  print(origin)
  X <- felicitacionesFB %>% 
    .[origen == origin] %>% 
    # .[, message := gsub("[[:punct:]]", "", message)] %>% 
    .[, c("message")]


  Y <- X %>% 
    tmMaker
  
  # XYT = Y %>%  TermDocumentMatrix()
  # XYT = slam::row_sums(XYT)
  # XYT = data.frame(palabra = XYT %>%  names, numero= XYT %>%  as.numeric)
  
  # x11()
  # wordcloud(Y)
  # wordcloud(Y, max.words = 100, scale= c(3, 1),
  #            colors = colores[2:length(colores)], min.freq = 0.05)

  # text(x = 0.5, y = 1, labels = origin)
})




lapply(origenes, function(origin){
  X <- quejasFB %>%
    .[origen==origin] %>%
    .[, message := gsub("[[:punct:]]", "", message)] %>%
    .[, "message"] 
  X <- tmMaker(X)
  
  x11()
  wordcloud(X, max.words = 100, scale= c(8, 1),
              colors = colores[2:length(colores)], min.freq = 0.05)

  text(x = 0.5, y = 1, labels = origin)
})

lapply(origenes, function(origin){
  X <- sugerenciasFB %>% 
    .[origen==origin] %>% 
    .[, message := gsub("[[:punct:]]", "", message)] %>% 
    .[, "message"] 
  
  X <- tmMaker(X)
  x11()
  wordcloud(X, max.words = 500, scale= c(8, 1), 
            colors = colores[2:length(colores)], min.freq = 0.00001)
  
  text(x = 0.5, y = 1, labels = origin)
})


lapply(origenes, function(origin){
  X <- ayudaFB %>% 
    .[origen==origin] %>% 
    .[, message := gsub("[[:punct:]]", "", message)] %>% 
    .[, "message"] 
  
  X <- tmMaker(X)
  x11()
  wordcloud(X, max.words = 500, scale= c(8, 1), 
            colors = colores[2:length(colores)], min.freq = 0.00001)
  
  text(x = 0.5, y = 1, labels = origin)
})

## Relaciones de palabras 

lapply(origenes, function(origin){
  X <- quejasFB %>%
    .[origen==origin] %>%
    .[, message := gsub("[[:punct:]]", "", message)] %>%
    .[, "message"] 
  
  X  <- tmMaker(X)
  
  t1 <- TermDocumentMatrix(X)
  
  if(dim(t1)[2]> 300){
    desparsedt1  <- removeSparseTerms(t1, 0.93) 
    if(dim(t1)[2]> 800){
      desparsedt1  <- removeSparseTerms(t1, 0.85)
    }
  }else{
    desparsedt1  <- removeSparseTerms(t1, 0.95) 
  }
  
  distribucion <- dist(scale(desparsedt1))
  cluster      <- hclust(distribucion)
  
  x11()
  plot.phylo(as.phylo(cluster), type="fan", font=2, cex = 1,
             edge.width = 2, tip.color = colores[2:length(colores)])
  text(x = 0.5, y = 2, labels = origin)
  
})

lapply(origenes, function(origin){
  X <- ayudaFB %>%
    .[origen==origin] %>%
    .[, message := gsub("[[:punct:]]", "", message)] %>%
    .[, "message"] 
  X <- tmMaker(X)
  t1           <- TermDocumentMatrix(X)
  desparsedt1  <- removeSparseTerms(t1, 0.94)
  distribucion <- dist(scale(desparsedt1))
  cluster      <- hclust(distribucion)
   x11()
  plot.phylo(as.phylo(cluster), type="fan", font=2, cex = 1,
              edge.width = 2, tip.color = colores)
   text(x = 0.5, y = 2, labels = origin)
  
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

