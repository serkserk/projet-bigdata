##################### SHINY SERVER ###########################

######################### LIBRARY ############################

library(mongolite)

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

################### SERVER ##################

shinyServer( function(input, output,session) { 
  
  print("lancement de l'application")
  
})