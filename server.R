##################### SHINY SERVER ###########################

######################### LIBRARY ############################

library(mongolite)
library(leaflet)
library(dplyr)
library(ggplot2)
library(leaflet.extras)
library(fpp)
library(forecast)

################### CONNECTION TO MONGO #######################

cap = mongo(collection = "capteurs",
            db = "trafic",
            url = "mongodb://193.51.82.104:2343")

tra = mongo(collection = "trafic",
            db = "trafic",
            url = "mongodb://193.51.82.104:2343")


################## USEFUL FUNCTION ###################


# Convert a date ith format for mongo

convertDateForMongo <- function(date) {
  return (strftime(date , "%Y-%m-%dT%H:%M:%SZ"))
}

# Chargement des coordonnées gps des capteurs
loadcapteurs <- function() {
  capteurs = cap$find(fields = '{ "geometry.coordinates": 1, "fields.id_arc_tra" : 1}')
  idscapteurs <-
    capteurs %>% select(fields, geometry) %>% distinct(fields, .keep_all = TRUE)
  idscapteurs <- na.omit(idscapteurs)
  
  idscapteurs$lng = sapply(idscapteurs$geometry$coordinates,
                           function(e) {
                             if (!is.null(e))
                               return(e[1])
                             
                             return(NA)
                           })
  
  idscapteurs$lat = sapply(idscapteurs$geometry$coordinates,
                           function(e) {
                             if (!is.null(e))
                               return(e[2])
                             
                             return(NA)
                           })
  
  geo <-
    data.frame(idscapteurs$fields$id_arc_tra,
               idscapteurs$lng,
               idscapteurs$lat)
  geo <- na.omit(geo)
  geo$ids <- geo$idscapteurs.fields.id_arc_tra
  geo$idscapteurs.fields.id_arc_tra <- NULL
  colnames(geo) <- c("lng", "lat", "id")
  
  return(geo)
  
}

loadStationInfo <- function(station_number) {
  q <- paste0('{"id" : ' , station_number , '}')
  return(tra$find(q))
  
}

# Chargement du nombre de voiture par ans par voiture :
loadTrafficYear <- function() {
  # Compte le traffic par année par station
  
  agg = tra$aggregate(
    '[
    { "$group": {
    "_id": {
    "id" : "$id"
    },
    "debit": { "$sum": "$debit" },
    "taux" : { "$avg": "$taux" }
    
    }}
    
    ]'
  )
  
  # Format the data frame
  
  df <- as.data.frame(agg)
  df <- data.frame(df$`_id`$id, df$debit, df$taux)
  colnames(df) <-  c("id", "debit", "taux")
  
  print("données chargées")
  return(df)
  
  }

# Chargement du débit par mois et par année
loadYearMonthDebit <- function() {
  agg = tra$aggregate(
    '[
    { "$group": {
    "_id": {
    "annee": { "$year": "$date" },
    "mois": { "$month": "$date" }
    },
    "debit": { "$avg": "$debit" }
    }}
    ]'
  )
  
  res <- na.omit(agg)
  df <- data.frame(res$`_id`$annee, res$`_id`$mois, res$debit)
  colnames(df) <- c("annee", "mois", "debit")
  return(df)
  
  }

loadHourDebit <- function() {
  agg = tra$aggregate('[
                      { "$group": {
                      "_id": {
                      "heure": { "$hour": "$date" }
                      },
                      "debit": { "$avg": "$debit" }
                      }}
                      ]')
  
  res <- na.omit(agg)
  df <- data.frame(res$`_id`$heure, res$debit)
  colnames(df) <- c("heure", "debit")
  return(df)
  
}


loadDebitDays <- function() {
  agg = tra$aggregate(
    '[
    { "$group": {
    "_id": {
    "jour": { "$dayOfWeek": "$date" },
    "heure" : { "$hour" : "$date"}
    },
    "debit": { "$avg": "$debit" }
    }}
    ]'
  )
  
  res <- na.omit(agg)
  df <- data.frame(res$`_id`$jour, res$debit)
  colnames(df) <- c("jour", "debit")
  return(df)
  
  }

PredictDebit <- function(number) {
  query = paste(
    '[
    {"$match" : { "id" :',
    number,
    '}},
    { "$group": {
    "_id": {
    "id" : "number",
    "annee": { "$year": "$date" },
    "mois": { "$month": "$date" }
    },
    "taux": { "$avg": "$debit" }
    }}
    ]'
  )
  
  trafic_agg = tra$aggregate(query)
  
  trafic_agg <-
    data.frame(trafic_agg$`_id`$annee,
               trafic_agg$`_id`$mois,
               trafic_agg$taux)
  colnames(trafic_agg) <- c('annee', 'mois', 'taux')
  
  trafic_agg <- trafic_agg[order(trafic_agg$annee, trafic_agg$mois), ]
  
  z <- ts(trafic_agg$taux,
          frequency = 12,
          start = c(2013, 1))
  #on met la date en index
  ts1 = ts(z, frequency = 12)
  fit <- auto.arima(ts1)
  
  }


############ VARIABLES GLOBALES  ################

# A GARDER MAIS PREND BEAUCOUP DE TEMPS A CHARGER

#TRAFFIC_ANNEE <- loadTrafficYear()
#DEBITMOY <- loadYearMonthDebit()
#DEBITJOUR <- loadDebitDays()

# PLUS RAPIDE AVEC LES CSV SAUVEGARDE :

TRAFFIC_ANNEE <- read.csv("debittaux.csv", row.names = 1)
DEBITMOY <- read.csv("DEBITMOY.csv", row.names = 1)
DEBITJOUR <- read.csv("debitjour.csv", row.names = 1)
DEBITHEURE <- read.csv("DEBITHEURE.csv", row.names = 1)

################### SERVER ##################

shinyServer(function(input, output, session) {
  ######### VARIABLES SERVER  #######
  
  # Carte de Paris
  paris = leaflet() %>% addTiles %>% setView(lng = 2.34, lat = 48.855, zoom = 12) %>%
    addTiles() %>%
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite)
  
  # Liste des ids des stations :
  listeIds <- tra$distinct("id")
  
  print("lancement de l'application")
  
  # Capteurs
  geo <- loadcapteurs()
  
  # Carte de paris
  output$Carte_capteurs <- renderLeaflet({
    paris
  })
  
  ############ INPUT / OUTPUT ############
  
  ############ ONGLET 1 CARTE CAPTEURS ##########
  
  # Event when a stationis clicked
  observeEvent(input$Carte_capteurs_marker_click, {
    click <- input$Carte_capteurs_marker_click
    
    # Load the sation info of the clicked station
    info <- loadStationInfo(click$id)
    
    # Check si des données sont disponibles :
    
    if (info$id[[1]] %in% listeIds) {
      output$series <- renderPlot({
        info %>%
          mutate(annee = format(date, "%Y"),
                 mois = format(date, "%m")) %>%
          group_by(mois, annee) %>%
          
          summarise(moy_taux = mean(tauxNum, na.rm = TRUE)) %>%
          ggplot(aes(mois, moy_taux, col = annee, group = annee)) +
          geom_line() +
          theme_classic()
      })
      
      
      
      output$mois <- renderPlot({
        info %>% mutate(heure = format(date, "%H"),
                        mois = format(date, "%m")) %>%
          group_by(heure, mois) %>%
          summarise(moy = mean(tauxNum, na.rm = TRUE)) %>%
          ggplot(aes(heure, moy, col = mois, group = mois)) +
          geom_line() +
          theme_classic()
        
      })
      
      output$jour <- renderPlot({
        info %>% mutate(heure = format(date, "%H"),
                        jour = format(date, "%A")) %>%
          group_by(heure, jour) %>%
          summarise(moy = mean(tauxNum, na.rm = TRUE)) %>%
          ggplot(aes(heure, moy, col = jour, group = jour)) +
          geom_line() +
          theme_classic()
        
      })
      
      output$prediction <- renderPlot({
        fit <- PredictDebit(click$id)
        plot(forecast(fit, h = 12), ylab = "débit", main = "prediction du débit")
        
      })
      
      
    }
    
  })
  
  
  observeEvent(input$top, {
    # Connection a la carte
    proxy <- leafletProxy("Carte_capteurs")
    
    proxy %>% clearHeatmap()
    
    # TABLE DES DONNEES TRIER
    merged <- merge(geo, TRAFFIC_ANNEE)
    ord <- merged[order(merged$taux), ]
    ord <- ord[which(ord$taux > 0), ]
    
    proxy %>% clearMarkers()
    
    if (input$top == "Station avec le plus de traffic") {
      print("test")
      top <- ord[(nrow(ord) - 49):(nrow(ord)), ]
      print(nrow(ord))
      
    }
    
    else if (input$top == "Station avec le moins de traffic") {
      top <- ord[1:50, ]
      
    }
    
    
    else{
      top <- ord
    }
    
    
    popup <- paste0("<strong> Station Id :  </strong>", top$id)
    proxy %>% addCircleMarkers(
      data = top,
      lat = ~ lat,
      lng = ~ lng,
      radius = 1,
      opacity = 0.5,
      layerId = ~ id,
      popup = popup,
      weight = 5,
      color = "blue"
    )
    
    
    if (input$top == "heatmap traffic") {
      proxy %>% clearMarkers()
      proxy %>% addHeatmap(data = top, intensity = ~ taux / 100)
    }
    
    output$tabTop <- renderDataTable({
      top
      
    })
    
    
  })
  
  ################ ONGLET 2 ###########
  
  observeEvent(input$choix, {
    if (input$choix == "par annee") {
      output$debitmoyen <- renderPlot({
        annee = c("2013", "2014", "2015", "2016")
        
        tibble(mois = DEBITMOY$annee, taux = DEBITMOY$debit) %>%
          group_by(mois) %>%
          summarise(taux = mean(taux)) %>%
          ggplot(aes(mois, taux)) +  geom_bar(stat = "identity") + coord_cartesian(ylim = c(770, 865)) +
          ggtitle("Moyenne du débit par annee") +
          xlab("annee")
        
      })
    }
    
    else if (input$choix == "par jour") {
      output$debitmoyen <- renderPlot({
        jour = c("dimanche",
                 "lundi",
                 "mardi",
                 "mercredi",
                 "jeudi",
                 "vendredi",
                 "samedi")
        
        tibble(mois = DEBITJOUR$jour, taux = DEBITJOUR$debit) %>%
          group_by(mois) %>%
          summarise(taux = mean(taux)) %>%
          ggplot(aes(mois, taux)) + xlab("jour de la semaine") + coord_cartesian(ylim = c(600, 1000)) +
          ggtitle("Moyenne du débit par jours") +
          geom_bar(stat = "identity") +
          scale_x_continuous(breaks = 1:7)
        
      })
    }
    
    else if (input$choix == "par mois") {
      output$debitmoyen <- renderPlot({
        month = c(
          "Janvier",
          "Fevrier",
          "Mars",
          "Avril",
          "Mai",
          "Juin",
          "Juillet",
          "Aout",
          "Septembre",
          "Octobre",
          "Novembre",
          "Decembre"
        )
        
        tibble(mois = DEBITMOY$mois, taux = DEBITMOY$debit) %>%
          group_by(mois) %>%
          summarise(taux = mean(taux)) %>%
          ggplot(aes(mois, taux)) + coord_cartesian(ylim = c(600, 900)) +
          ggtitle("Moyenne du débit par jours") +
          geom_bar(stat = "identity") +
          scale_x_continuous(breaks = 1:12)
        
        
      })
    }
    
    else if (input$choix == "par heure") {
      output$debitmoyen <- renderPlot({
        tibble(mois = DEBITHEURE$heure, taux = DEBITHEURE$debit) %>%
          group_by(mois) %>%
          summarise(taux = mean(taux)) %>%
          ggplot(aes(mois, taux)) + xlab("heure de la journée") +
          ggtitle("Moyenne du débit par heure") +
          geom_bar(stat = "identity") +
          scale_x_continuous(breaks = 0:23)
        
        
      })
    }
    
  })
  
  
  output$countCap <- renderValueBox({
    valueBox(
      value = cap$count(),
      subtitle = "Total capteurs",
      icon = icon("list"),
      color = "blue"
    )
  })
  
  output$countTra <- renderValueBox({
    valueBox(tra$count(),
             "Total captures",
             icon =  icon("list"),
             color = "yellow")
  })
  
  
  
})
