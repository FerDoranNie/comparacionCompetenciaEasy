####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

library(magrittr) # magia magia pokemon
c("data.table", "dplyr", "ggplot2", "tidyr", "XML",
  "httr", "rvest", "stringi", "stringr", "tm", "wordcloud", "ape") %>% 
  sapply(require, character.only=T)


colores <- palettetown::pokepal("quilava", spread=12) 

##Descargando los comentarios de la playstore 
# link_Uber <- "https://play.google.com/store/apps/details?id=com.ubercab&hl=en"
link_99     <- "https://play.google.com/store/apps/details?id=com.taxis99&hl=es"
link_Uber   <- "https://play.google.com/store/apps/details?id=com.ubercab&hl=es"
link_Easy   <- "https://play.google.com/store/apps/details?id=br.com.easytaxi&hl=es"
link_Cabify <- "https://play.google.com/store/apps/details?id=com.cabify.rider&hl=es"

link_Yaxxi  <- "https://play.google.com/store/apps/details?id=com.zebstudios.yaxi&hl=es"


# lineas_99   <- readLines(link_99)
# lineas_Uber <- readLines(link_Uber)
lineas_99     <- read_html(link_99)
lineas_Uber   <- read_html(link_Uber)
lineas_Easy   <- read_html(link_Easy)
lineas_Cabify <- read_html(link_Cabify)

lineas_99     <-   lineas_99       %>%  htmlParse(asText = T)
lineas_Uber   <-   lineas_Uber     %>%  htmlParse(asText = T)
lineas_Easy   <-   lineas_Easy     %>%  htmlParse(asText = T)
lineas_Cabify <-   lineas_Cabify   %>%  htmlParse(asText = T)

##Resumen general 

# xpathSApply(lineas_99, 
#             "//*[@class='review-date']/text()", 
#             xmlValue)

paginas <- list(lineas_Easy, lineas_99, lineas_Uber, lineas_Cabify)

resumen <- paginas  %>% 
  lapply(function(app){
    nombre <- xpathSApply(app,  "//*[@class='id-app-title']", xmlValue)
    
    numeroDescargas <-  xpathSApply(app, 
                                    "//*[@itemprop='numDownloads']/text()", 
                                    xmlValue)
    
    score   <-  xpathSApply(app, "//*[@class='score']/text()", xmlValue)
    score   <-  gsub(",", ".", score)
    score   <-  as.numeric(score)
    reviews <-  xpathSApply(app, "//*[@class='reviews-num']/text()", xmlValue)
    reviews <-  gsub("[.]", "", reviews)
    reviews <-  as.character(reviews)
    reviews <-  as.numeric(reviews)
    X <- data.frame(nombre, numeroDescargas, score, reviews)
    return(X)
  }) %>%  do.call("rbind", .)


resumen <- resumen %>% 
  data.table %>% 
  .[order(-score)]



# Comentarios -------------------------------------------------------------
Comentarios <- lapply(paginas, function(app){
  nombre <- xpathSApply(app,  "//*[@class='id-app-title']", xmlValue)
  comentarios1 <- xpathSApply(app, "//div[@class='review-text']",
                              xmlValue)
  comentarios2 <- xpathSApply(lineas_99,
                              "//div[@class='review-body with-review-wrapper']",
                              xmlValue)
  comentarios2 <- gsub("Opinión completa", "", comentarios2)
  comentarios<- c(comentarios1, comentarios2)
  X <- data.frame(comentarios = comentarios)
  X <- X %>%  
    data.table %>% 
    .[, origen := nombre]
  return(X)
}) %>% do.call("rbind", .)



# comentariosA99    <- xpathSApply(lineas_99, "//div[@class='review-text']",
#                               xmlValue)
# 
# comentariosB99    <- xpathSApply(lineas_99,
#                               "//div[@class='review-body with-review-wrapper']",
#                               xmlValue)

# comentariosB99    <- xpathSApply(lineas_99, 
#                               "//div[@class='review-body with-review-wrapper']/text()", 
#                               xmlValue)

# comentariosAUber  <- xpathSApply(lineas_Uber, "//div[@class='review-text']",
#                               xmlValue)
# 
# comentariosBUber  <- xpathSApply(lineas_Uber, 
#                               "//div[@class='review-body with-review-wrapper']", 
#                               xmlValue)


# read_html(link_99) %>% html_nodes (".review-body") %>% 
#   lapply(function(s){html_children(s) %>%  html_text})
# 
# read_html(link_99) %>% html_nodes (".document-subtitles") %>% 
#   html_text

apps <- unique(Comentarios$origen)

lapply(apps, function(app){
  test <- Comentarios %>% 
    data.table %>% 
    .[origen == app] %>% 
    .[, comentarios := gsub("[[:punct:]]", "", comentarios)] %>% 
    .[, "comentarios"] %>%
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
  wordcloud(test, max.words = 200, scale= c(7, 1), 
            colors = colores[2:length(colores)], min.freq = 0.001)
  text(x = 0.5, y = 1, labels = app)
  
})

lapply(apps, function(app){
  test <- Comentarios %>% 
    data.table %>% 
    .[origen == app] %>% 
    .[, comentarios := gsub("[[:punct:]]", "", comentarios)] %>% 
    .[, "comentarios"] %>%
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

    t1           <- TermDocumentMatrix(test)  
    desparsedt1  <- removeSparseTerms(t1, 0.96)
    distribucion <- dist(scale(desparsedt1))
    cluster      <- hclust(distribucion)
    x11()
    # plot(cluster)
    plot.phylo(as.phylo(cluster), type="fan", font=2, cex = 1,
               edge.width = 2, tip.color = colores)
    text(x = 0.5, y = 2, labels = app)
    
    nombre1 <- strsplit(app, split = " ") %>% 
      unlist %>%
      paste(collapse="_")
    
    png(filename = paste("~/local/comparacionEasy_Uber_99/imagenes/", nombre1, ".png", sep=""), 
        width = 1200, height = 800, res = 100 )
    plot(cluster)
    dev.off()
})





# Comentarios del playstore --------------------------------------
conductores <- list.files("~/local/comparacionEasy_Uber_99/conductores/", 
                          pattern = "*.csv", full.names = T)


pasajeros   <- list.files("~/local/comparacionEasy_Uber_99/consumidores/", 
                          pattern = "*.csv", full.names = T )


conductores <- lapply(conductores, function(d){
  z <- read.table(d, header=T, sep=",", fileEncoding  = "UCS-2LE", fill = T)
  return(z)
}) %>% 
  rbindlist

pasajeros <- lapply(pasajeros, function(d){
  z <- read.table(d, header=T, sep=",", fileEncoding  = "UCS-2LE", fill = T)
  return(z)
}) %>% 
  rbindlist


titulosPasajeros <-   pasajeros %>% 
  .[, Review.Title := gsub("[[:punct:]]", "", Review.Title)] %>% 
  .[, "Review.Title"] %>%
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


textoPasajeros <-   pasajeros %>% 
  .[, Review.Text := gsub("[[:punct:]]", "", Review.Text)] %>% 
  .[, "Review.Text"] %>%
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


titulosConductores <-   conductores %>% 
  .[, Review.Title := gsub("[[:punct:]]", "", Review.Title)] %>% 
  .[, "Review.Title"] %>%
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


textoConductores <-   conductores %>% 
  .[, Review.Text := gsub("[[:punct:]]", "", Review.Text)] %>% 
  .[, "Review.Text"] %>%
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
wordcloud(textoConductores, max.words = 500, scale= c(8, 1), 
          colors = colores[2:length(colores)], min.freq = 0.00001)

