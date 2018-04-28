##################### SHINY SERVER ###########################

######################### LIBRARY ############################

library(mongolite)
library(leaflet)
library(dplyr)

################### CONNECTION TO MONGO #######################

cap = mongo(collection = "capteurs", db = "trafic", 
            url = "mongodb://193.51.82.104:2343")

tra = mongo(collection = "trafic", db = "trafic", 
            url = "mongodb://193.51.82.104:2343")


################## USEFUL FUNCTION ###################


# Convert a date ith format for mongo

convertDateForMongo <- function(date){
  return (strftime(date , "%Y-%m-%dT%H:%M:%SZ"))
}

# Chargement des coordonnées gps des capteurs
loadcapteurs <- function(){
  
  capteurs = cap$find(fields = '{ "geometry.coordinates": 1, "fields.id_arc_tra" : 1}')
  idscapteurs <- capteurs %>% select(fields,geometry) %>% distinct(fields,.keep_all = TRUE)
  idscapteurs <- na.omit(idscapteurs)
  
  idscapteurs$lng = sapply(idscapteurs$geometry$coordinates, 
                           function(e) { 
                             if (!is.null(e)) return(e[1]); 
                             return(NA) 
                           })
  
  idscapteurs$lat = sapply(idscapteurs$geometry$coordinates, 
                           function(e) { 
                             if (!is.null(e)) return(e[2]); 
                             return(NA) 
                           })
  
  geo <- data.frame(idscapteurs$fields$id_arc_tra,idscapteurs$lng,idscapteurs$lat)
  geo <- na.omit(geo)
  geo$idscapteurs.fields.id_arc_tra <- NULL
  rownames(geo) <- geo$idscapteurs.fields.id_arc_tra
  colnames(geo) <- c("lng","lat")
  
  return(geo)
  
}

################### SERVER ##################

shinyServer( function(input, output,session) { 
  
  # Carte de Paris
  paris = leaflet() %>% addTiles %>% setView(lng = 2.34, lat = 48.855, zoom = 12) %>% addTiles()
  
  print("lancement de l'application")
  
  # carte des capteurs
  
  output$Carte_capteurs <- renderLeaflet({
    
    loadcapteurs()
    geo <- loadcapteurs()
    paris %>% addCircleMarkers(data = geo, radius = 1)
    
  })
  
})