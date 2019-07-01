library("leaflet")
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
df <-read.csv("www/csv/sampledata.csv")
names(df)[names(df) == "decimalLatitude"] <- "latitude"
names(df)[names(df) == "decimalLongitude"] <- "longitude"



m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(data=df, popup="The birthplace of R")
m
