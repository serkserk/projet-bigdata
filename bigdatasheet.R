library(mongolite)
library(leaflet)
library(dplyr)

cap = mongo(collection = "capteurs",
            db = "trafic",
            url = "mongodb://193.51.82.104:2343")

tra = mongo(collection = "trafic",
            db = "trafic",
            url = "mongodb://193.51.82.104:2343")


trafic_agg = tra$aggregate(
  '[
  { "$group": {
  "_id": {
  "id" : "number",
  "annee": { "$year": "$date" },
  "mois": { "$month": "$date" },
  "jour": { "$dayOfWeek": "$date" }
  },
  "nb": { "$sum": 1 }
  }}
  ]'
)

tra$count('{"id" : 9}')

tra$find(query = '{"id" : 1, "date": { "$lte" : { "$date" : "2016-01-01T00:00:00Z" }}}',
         limit = 1000)

capteurs = cap$find(fields = '{ "geometry.coordinates": 1, "fields.id_arc_tra" : 1}')


paris <- leaflet() %>% addTiles %>%
  setView(lng = 2.34, lat = 48.855, zoom = 12)

paris %>%
  addCircles(
    data = capteurs,
    lng = ~ 2.34,
    lat = ~ 48.855 ,
    color = "darkred"
  ) %>%
  addProviderTiles(providers$Thunderforest.SpinalMap)


###################" ANALYSE POUR 2 SEMAINE DE DONNEES ##################

###### Request data from Mongo by dates

# Date range :

begin <- "2016-06-06 00:00:00 CEST"
end <- "2016-06-13 00:00:00 CEST"

transformated_begin <- strftime(begin , "%Y-%m-%dT%H:%M:%SZ")
transformated_end <- strftime(end , "%Y-%m-%dT%H:%M:%SZ")

query <-
  paste0(
    '{"date" : { "$lt" : { "$date": "',
    transformated_end,
    '" } , "$gte" : { "$date" : "',
    transformated_begin,
    '" }}}'
  )

res2days <- tra$find(query, sort = '{"date" : 1 }')

res <- split(res2days, res2days$date)

df <- c()
for (e in res) {
  df <- cbind(df, e$debit)
}

df <- as.data.frame(df)
df[df == ""] <- NA

res <- lapply(df, strtoi)
res <- as.data.frame(res)

plot.ts(t(res[1, ]))

for (i in 1:ncol(res)) {
  res[is.na(res[, i]), i] <- mean(res[, i], na.rm = TRUE)
}

ids <- res2days %>% distinct(id)
rownames(res) <- ids[, 1]

km <- kmeans(res, 4, nstart = 100)
plot.ts(t(km$centers))

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
geo$idscapteurs.fields.id_arc_tra <- NULL
rownames(geo) <- geo$idscapteurs.fields.id_arc_tra
colnames(geo) <- c("lng", "lat")

merged <- merge(geo, res, by = "row.names")
rownames(merged) <- merged$Row.names
merged$Row.names <- NULL

s <- scale(merged)

km <- kmeans(s, 3, nstart = 10)
km <- kmeans(s, 3, nstart = 10)

plot.ts(t(km$centers))

pal <-
  colorNumeric(c("red", "green", "blue", "yellow", "grey", "orange", "black"),
               1:4)
merged <- as.data.frame(merged)

paris = leaflet() %>% addTiles %>%
  setView(lng = 2.34, lat = 48.855, zoom = 12) %>%
  addProviderTiles(providers$Stamen.TonerLite)

paris %>%
  addCircleMarkers(
    data = merged,
    lng = ~ lng,
    lat = ~ lat,
    weight = 1,
    radius = 20 * merged[, 12] / max(merged[, 12]),
    fillOpacity = 1,
    fillColor = pal(clust2@cluster)
  )




install.packages("dtwclust")
library(dtwclust)

clust1 = tsclust(
  res,
  type = "partitional",
  k = 2,
  distance = "dtw",
  centroid = "pam",
  trace = TRUE,
  args = tsclust_args(dist = list(window.size = 2))
)
plot(clust1)


clust2 = tsclust(res, type = "hierarchical", trace = TRUE)
plot(clust2)
plot.ts(as.numeric(unlist(clust2@centroids[1])))
plot.ts(as.numeric(unlist(clust2@centroids[2])))
plot(clust2, type = "sc")

plot(ts(as.data.frame(clust2@centroids)))


save.image(file = "data.RData")
load("data.RData")

paris = leaflet() %>% addTiles %>%
  setView(lng = 2.34, lat = 48.855, zoom = 12) %>%
  addProviderTiles(providers$Stamen.TonerLite)

paris %>%
  addCircleMarkers(
    data = merged,
    lng = ~ 2.34,
    lat = ~ 48.855,
    weight = 1,
    radius = 20 * merged[, 12] / max(merged[, 12]),
    fillOpacity = 1,
    fillColor = pal(clust2@cluster)
  )
