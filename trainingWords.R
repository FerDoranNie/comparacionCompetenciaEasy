####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

library(magrittr)
c("data.table", "RTextTools", "e1071") %>% 
  sapply(require, character.only=T)


palabras = read.csv("~/local/palabrasEspaniol/SpanishDAL-v1.2/meanAndStdev.csv",
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
  .[, c("palabra", "sentimientoAgrado")]


palabrasEntrenamiento
