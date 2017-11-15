####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)"
#                                      /|
####################################

library(magrittr)
c("data.table", "dplyr", "Rfacebook", "twitteR") %>% 
  sapply(require, character.only=T)

source("~/local/claves/keys_redesSociales.R") ### Archivo donde están las claves y accesos de redes sociales

# Conexión a Facebook -----------------------------------------------------
fb_oauth <- fbOAuth(app_id = app_id, 
                    app_secret = app_secret, 
                    extended_permissions = TRUE)
save(fb_oauth, file= "fb_oauth")
load("~/fb_oauth")



# Conexion Twitter --------------------------------------------------------
setup_twitter_oauth(key, secret, access_token, secret_token)


# ids ---------------------------------------------------------------------
idsRedes <- read.csv("~/local/idsRedesSociales.csv", header = T)

idsFB <- idsRedes %>% 
  data.table %>% 
  .[ red=="facebook"] %>% 
  .$id

idsTW <- idsRedes %>% 
  data.table %>% 
  .[ red=="twitter"] %>% 
  .$id


# Datos Facebook ----------------------------------------------------------
posteosFB <- lapply(idsFB, function(red){
  datosRed <- getPage(red, token = fb_oauth, n = 1e5, since = "2017-01-01",
          until = Sys.Date(), feed = F, reactions = T, verbose = T)
  print(red)
  return(datosRed)
})

posteosFB <- do.call("rbind", posteosFB)

idsPosteos <- posteosFB$id
length(idsPosteos)


comentariosFB <- lapply(idsPosteos, function(posteo){
  datoPosteo <- tryCatch(
    getPost(post = posteo, token = fb_oauth, n = 200),
    error = function(e){return(NULL)}
  )
  print(posteo)
  if(!is.null(datoPosteo)){
    comentarioPost <- datoPosteo$comments
    comentarioPost <- comentarioPost %>% 
      data.table %>% 
      .[, idPosteo := posteo]
    return(comentarioPost)  
  }else{
    return(NULL)
  }
})

comentariosFB <- do.call("rbind", comentariosFB)


posteosFB %>% 
  write.csv("~/local/comparacionEasy_Uber_99/data/posteosFB.csv", row.names = F)


comentariosFB %>% 
  write.csv("~/local/comparacionEasy_Uber_99/data/comentariosFB.csv", row.names = F)




# Datos Twitter  ----------------------------------------------------------
idsTW <- c(idsTW[-length(idsTW)], "715649691279560705")

tweets <- lapply(idsTW, function(twitter){
  datosRed <- tryCatch(
    userTimeline(user = twitter, n = 3200, includeRts = F),
    error = function(e){return(NULL)}
  )
  print(twitter)
  if(!is.null(datosRed)){
    datosRed <- twListToDF(datosRed)
    return(datosRed)
  }else{
    return(NULL)
  }
  # datosRed <- userTimeline(user = twitter, n = 2, includeRts = F)
  # print(twitter)
  # return(datosRed)
})

tweets <- do.call("rbind", tweets)

tweets %>% 
  write.csv("~/local/comparacionEasy_Uber_99/data/tweets.csv", row.names = F)







