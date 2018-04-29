##################### SHINY SERVER ###########################

######################### LIBRARY ############################

library(mongolite)
library(leaflet)
library(dplyr)
library(ggplot2)

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
  geo$ids <- geo$idscapteurs.fields.id_arc_tra
  geo$idscapteurs.fields.id_arc_tra <- NULL
  colnames(geo) <- c("lng","lat","id")
  
  return(geo)
  
}

loadStationInfo <- function(station_number){
  
  q <- paste0('{"id" : ' , station_number , '}')
  return(tra$find(q))
  
}

# Chargement du nombre de voiture par ans par voiture :
loadTrafficYear <- function(){
  
  # Compte le traffic par année par station 
  
  agg = tra$aggregate(
    '[
  { "$group": { 
    "_id": { 
      "id" : "$id",
      "annee": { "$year": "$date" }
    }, 
    "traffic": { "$sum": "$debit" }
  }}

  ]')
  
  return(agg)
  
}

############ VARIABLES GLOBALES  ################

# A GARDER MAIS PREND BEAUCOUP DE TEMPS A CHARGER
#TRAFFIC_ANNEE <- loadTrafficYear()

################### SERVER ##################

shinyServer( function(input, output,session) { 
  
  # Carte de Paris
  paris = leaflet() %>% addTiles %>% setView(lng = 2.34, lat = 48.855, zoom = 12) %>% 
    addTiles() %>%
    addProviderTiles(providers$CartoDB.DarkMatter)
  
  # Liste des ids des stations : 
  listeIds <- tra$distinct("id")
  
  print("lancement de l'application")
  
  # carte des capteurs
  
  output$Carte_capteurs <- renderLeaflet({
    
    geo <- loadcapteurs()

    popup <- paste0("<strong> Station Id :  </strong>",geo$id)
    paris %>% addCircleMarkers(data = geo,
                               lat = ~lat, 
                               lng = ~lng,
                               radius = 1,
                               opacity = 1,
                               layerId = ~id,
                               popup = popup,
                               weight = 5)
    
  })
  
  observeEvent(input$Carte_capteurs_marker_click,{
    
    click <- input$Carte_capteurs_marker_click
    
    # Load the sation info of the clicked station 
    info <- loadStationInfo(click$id)
    if (info$id[[1]] %in% listeIds){
      
      output$series <- renderPlot({

        print(info)
        info %>% 
          mutate(annee = format(date, "%Y"), 
                 jour = format(date, "%j")) %>%
          ggplot(aes(jour, tauxNum, col = annee)) +
          geom_line() +
          theme_classic()
        
    })
      
    }
    
  })
  

    
  })
  
  
  
  
})