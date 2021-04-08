getSigel <- function () {
  hbz <- read.csv2(file="data/hbzall.txt", sep = "\t")
   
  url <- "https://sigel.staatsbibliothek-berlin.de/api/hydra/resource/"
  state <- c()
  count <- c()
  for (i in 1:length(hbz$Sigel)) {
    cursigel <- hbz$Sigel[[i]]
    curcount <- hbz$Count[[i]]
    res = GET(paste0(url,cursigel))
    data = fromJSON(rawToChar(res$content))
    curstate <- data$addressRegion
    print (paste(i,curstate))
    state <- c(state, curstate)
    count <- c(count, curcount)
  }
  resframe <- data.frame (
    State = state,
    Count = count
  )
  save(resframe, file = "resframe.rda")
}


createWorldMapOld <- function () {

  par(mai=c(0,0,0.2,0),xaxs="i",yaxs="i")

  #getting example data and joining to a map
  cd <- read.csv2("data/ccodes.csv", sep = " ")
  cd$Count <- cd$Count
  sPDF <- rworldmap::joinCountryData2Map(cd
                                  , joinCode = "ISO2"
                                  , nameJoinColumn = "Country"
                                  , mapResolution="coarse"
                                  )
  mapParams <- rworldmap::mapCountryData(
    sPDF, 
    nameColumnToPlot="Count", 
    addLegend = FALSE  )
  
  
  do.call( rworldmap::addMapLegend, 
           c(mapParams, 
             legendWidth=0.5, 
             legendMar = 2,
             horizontal = TRUE))
}


createGermanMap <- function () {
  load("data/resframe.rda")
  brd <- geojsonio::geojson_read("data/json/blaender.json", what = "sp")
  
  staedte <- read.csv2(file="data/citdydates.csv", sep = "\t", fileEncoding = "UTF-8")
  laender <- read.csv2(file="data/Liste-Staedte-in-Deutschland.csv", fileEncoding = "UTF-8")
  
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
  
  for (i in 1:length(staedte$or_del_city)) {
    curcity <- staedte$or_del_city[[i]]
    curdate <- staedte$date.or_date_acquire.[[i]]
    if (grepl("2020", curdate)) {
      if (length(which(laender$Stadt==curcity))==1) {
        state <- laender[which(laender$Stadt==curcity),][[4]]
        curcount <- laendercount$values[which(laendercount$key==state)]
        laendercount$values[which(laendercount$key==state)] <- curcount + 1
      }
    }
  }
  

  
  laendercount$values[laendercount$key == "Nordrhein-Westfalen"] =
    laendercount$values[laendercount$key == "Nordrhein-Westfalen"] + 
    sum(resframe$Count[resframe$State == "NRW"])
  
  laendercount$values[laendercount$key == "Baden-Württemberg"] =
    laendercount$values[laendercount$key == "Baden-Württemberg"] + 
    sum(resframe$Count[resframe$State == "BAW"])
  
  laendercount$values[laendercount$key == "Freistaat Bayern"] =
    laendercount$values[laendercount$key == "Freistaat Bayern"] + 
    sum(resframe$Count[resframe$State == "BAY"]) 
  
  laendercount$values[laendercount$key == "Niedersachsen"] =
    laendercount$values[laendercount$key == "Niedersachsen"] + 
    sum(resframe$Count[resframe$State == "NIE"]) 

  laendercount$values[laendercount$key == "Berlin"] =
    laendercount$values[laendercount$key == "Berlin"] + 
    sum(resframe$Count[resframe$State == "BER"]) 
  
  laendercount$values[laendercount$key == "Hessen"] =
    laendercount$values[laendercount$key == "Hessen"] + 
    sum(resframe$Count[resframe$State == "HES"]) 
  
  brd$density <- laendercount$values
  
  
  
  m <- leaflet(brd) %>% clearBounds()
  
  bins <- c(0, 1000, 2000, 3000, 4000, 5000, Inf)
  pal <- colorBin("YlOrRd", domain = brd$density, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Bestellungen",
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

createEuropeanMap <- function () {
  europe <- geojsonio::geojson_read("data/json/european-union-countries.geojson", what = "sp")
  eucountries <- read.csv2(file = "data/country-and-continent-codes-list.csv", sep = ",")
  ccodes <- read.csv2(file="data/ccodes.csv", sep = "\t")
  
  eulaendernamen <- eucountries[which(eucountries$Continent_Name=="Europe"),][[3]]
  zeroes <- sample(c(0), size = 57, replace=TRUE)
  eucount <- hash::hash(key=eulaendernamen, values=zeroes)
  
  for (i in 1:length(ccodes$or_del_country)) {
    curcountry <- ccodes$or_del_country[[i]]
    curdate <- ccodes$date.or_date_acquire.[[i]]
    if (grepl("2020", curdate)) {
      if (length(which(eucountries$Two_Letter_Country_Code==curcountry))==1) {
        continent <- eucountries[
          which(eucountries$Two_Letter_Country_Code==curcountry),][[1]]
        if (continent == "Europe") {
          curname <- eucountries[
            which(eucountries$Two_Letter_Country_Code==curcountry),][[3]]

          curcount <- eucount$values[which(eucount$key==curname)]
          eucount$values[which(eucount$key==curname)] <- curcount + 1
        }
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
  pal <- colorBin("YlOrRd", domain = europe$density, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Bestellungen",
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

createWorldMap <- function () {
  world <- geojsonio::geojson_read("data/json/world.json", what = "sp")
  
  ccodes <- read.csv2(file="data/ccodes.csv", sep = "\t")
  countries <- read.csv2(file="data/country-and-continent-codes-list.csv", sep = ",")
  wcodes <- world$ISO_A2
  zeroes <- sample(c(0), size = length(wcodes), replace=TRUE)
  worldcount <- hash::hash(key=wcodes, values=zeroes)
  
  for (i in 1:length(ccodes$or_del_country)) {
    curcountry <- ccodes$or_del_country[[i]]
    curdate <- ccodes$date.or_date_acquire.[[i]]
    if (grepl("2020", curdate)) {
      if (length(which(wcodes==curcountry))==1) {
        curcount <- worldcount$values[which(worldcount$key==curcountry)]
        worldcount$values[which(worldcount$key==curcountry)] <- curcount + 1
      }
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
  pal <- colorBin("YlOrRd", domain = world$density, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g Bestellungen",
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