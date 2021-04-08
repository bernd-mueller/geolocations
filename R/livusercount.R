createLivGermanMap <- function () {
  brd <- geojsonio::geojson_read("data/json/blaender.json", what = "sp")
  blaendercount <- read.csv2(file="data/livblaendercount.csv", sep = "\t", fileEncoding = "UTF-8")
  
  laendernamen <- c("Baden-Württemberg",
                    "Freistaat Bayern",
                    "Berlin",
                    "Brandenburg",
                    "Freie Hansestadt Bremen",
                    "Hamburg",
                    "Hessen",  
                    "Mecklenburg-Vorpommern",  
                    "Niedersachsen",  
                    "Nordrhein-Westfalen",  
                    "Rheinland-Pfalz",  
                    "Saarland",  
                    "Sachsen-Anhalt",  
                    "Freistaat Sachsen",
                    "Schleswig-Holstein",
                    "Freistaat Thüringen"
  )
  
  
  
  zeroes <- sample(c(0), size = 16, replace=TRUE)
  
  laendercount <- hash::hash(key=laendernamen, values=zeroes)
  
  laendercount$values[laendercount$key == "Baden-Württemberg"] = 
    blaendercount$Count[blaendercount$State == "Baden-Württemberg"]
  
  laendercount$values[laendercount$key == "Hessen"] = 
    blaendercount$Count[blaendercount$State == "Hessen"]
  
  laendercount$values[laendercount$key == "Berlin"] = 
    blaendercount$Count[blaendercount$State == "Berlin"]
  
  laendercount$values[laendercount$key == "Brandenburg"] = 
    blaendercount$Count[blaendercount$State == "Brandenburg"]
  
  laendercount$values[laendercount$key == "Freistaat Sachsen"] = 
    blaendercount$Count[blaendercount$State == "Sachsen"]
  
  laendercount$values[laendercount$key == "Freistaat Bayern"] = 
    blaendercount$Count[blaendercount$State == "Bayern"]
  
  laendercount$values[laendercount$key == "Nordrhein-Westfalen"] = 
    blaendercount$Count[blaendercount$State == "Nordrhein-Westfalen"]
  
  laendercount$values[laendercount$key == "Hamburg"] = 
    blaendercount$Count[blaendercount$State == "Hamburg"]
  
  laendercount$values[laendercount$key == "Mecklenburg-Vorpommern"] = 
    blaendercount$Count[blaendercount$State == "Mecklenburg-Vorpommern"]
  
  laendercount$values[laendercount$key == "Niedersachsen"] = 
    blaendercount$Count[blaendercount$State == "Niedersachsen"]
  
  laendercount$values[laendercount$key == "Freie Hansestadt Bremen"] = 
    blaendercount$Count[blaendercount$State == "Bremen"]
  
  laendercount$values[laendercount$key == "Freistaat Thüringen"] = 
    blaendercount$Count[blaendercount$State == "Thüringen"]
  
  laendercount$values[laendercount$key == "Saarland"] = 
    blaendercount$Count[blaendercount$State == "Saarland"]
  
  laendercount$values[laendercount$key == "Sachsen-Anhalt"] = 
    blaendercount$Count[blaendercount$State == "Sachsen-Anhalt"]
  
  laendercount$values[laendercount$key == "Rheinland-Pfalz"] = 
    blaendercount$Count[blaendercount$State == "Rheinland-Pfalz"]
  
  laendercount$values[laendercount$key == "Schleswig-Holstein"] = 
    blaendercount$Count[blaendercount$State == "Schleswig-Holstein"]
   
  
  brd$density <- laendercount$values
  
  
  
  m <- leaflet(brd) %>% clearBounds()
  
  bins <- c(0, 1000, 2000, 3000, 4000, 5000, Inf)
  
  pal <- colorBin("GnBu", domain = brd$density, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Benutzer",
    brd$name, brd$density
  ) %>% lapply(htmltools::HTML)
  
  m <- m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
              position = "bottomright")
  return (m)
}

createLivEuropeanMap <- function () {
  europe <- geojsonio::geojson_read("data/json/european-union-countries.geojson", what = "sp")
  eucountries <- read.csv2(file = "data/country-and-continent-codes-list.csv", sep = ",")
  ccodes <- read.csv2(file="data/livcountrycodecount.csv", sep = "\t")
  
  eulaendernamen <- eucountries[which(eucountries$Continent_Name=="Europe"),][[3]]
  zeroes <- sample(c(0), size = 57, replace=TRUE)
  eucount <- hash::hash(key=eulaendernamen, values=zeroes)
  
  for (i in 1:length(ccodes$ccode)) {
    curcountry <- ccodes$ccode[[i]]
    curcount <- ccodes$count[[i]]

    if (length(which(eucountries$Two_Letter_Country_Code==curcountry))==1) {
      continent <- eucountries[
        which(eucountries$Two_Letter_Country_Code==curcountry),][[1]]
      if (continent == "Europe") {
        curname <- eucountries[
          which(eucountries$Two_Letter_Country_Code==curcountry),][[3]]
        eucount$values[which(eucount$key==curname)] <- curcount
          
      }
    }
  }
  density <- c()
  for (name in europe$name) {
    if (name != "Ireland") {
      curcount <- eucount$values[grepl(name, eucount$key)]
      density <- c(density, curcount)
    } else {
      curcount <- eucount$values[which(eucount$key == "Ireland")]
      density <- c(density, curcount)
    }
  }
  
  europe$density <- density
  
  
  m <- leaflet(europe) %>% clearBounds()
  bins <- c(0, 1000, 2000, 3000, 4000, 5000, Inf)
  
  pal <- colorBin("GnBu", domain = europe$density, bins = bins)

  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Benutzer",
    europe$name, europe$density
  ) %>% lapply(htmltools::HTML)
  
  m <- m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
              position = "bottomright")  %>%
    setView(20, 56, 2.5)
}

createLivWorldMap <- function () {
  world <- geojsonio::geojson_read("data/json/world.json", what = "sp")
  
  ccodes <- read.csv2(file="data/livcountrycodecount.csv", sep = "\t")
  countries <- read.csv2(file="data/country-and-continent-codes-list.csv", sep = ",")
  wcodes <- world$ISO_A2
  zeroes <- sample(c(0), size = length(wcodes), replace=TRUE)
  worldcount <- hash::hash(key=wcodes, values=zeroes)
  
  for (i in 1:length(ccodes$ccode)) {
    curcountry <- ccodes$ccode[[i]]
    curcount <- ccodes$count[[i]]
    if (length(which(wcodes==curcountry))==1) {
      worldcount$values[which(worldcount$key==curcountry)] <- curcount 
    }
  }
  
  world$density <- worldcount$values
  
  cnames <- c()
  for (iso3 in world$ISO_A3) {
    curname <- countries$Country_Name[countries$Three_Letter_Country_Code == iso3][1]
    cnames <- c(cnames, curname)
    
  }
  world$name <- cnames
  m <- leaflet(world) %>% clearBounds()
  
  bins <- c(0, 1000, 2000, 3000, 4000, 5000, Inf)
  pal <- colorBin("GnBu", domain = europe$density, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Benutzer",
    world$name, world$density
  ) %>% lapply(htmltools::HTML)
  
  m <- m %>% addPolygons(
    fillColor = ~pal(density),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
              position = "bottomright")  %>%
    setView(20, 56, 1)
  
  return (m)
}