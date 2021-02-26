createWorldMap <- function () {

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
  brd <- geojsonio::geojson_read("data/json/blaender.json", what = "sp")
  
  staedte <- read.csv2(file="data/citdydates.csv", sep = "\t")
  laender <- read.csv2(file="data/Liste-Staedte-in-Deutschland.csv")
  
  laendernamen <- c("Baden-WÃ¼rttemberg",
                    "Freistaat Bayern",
                    " Berlin",
                    "Brandenburg",
                    "Freie Hansestadt Bremen",
                    " Hamburg",
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
        which(laendercount$key==" Berlin")
        curcount <- laendercount$values[which(laendercount$key==state)]
        laendercount$values[which(laendercount$key==state)] <- curcount + 1
      }
    }
  }
  
  brd$density <- laendercount$values
  
  
  
  m <- leaflet(brd) %>% clearBounds()
  
  bins <- c(0, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000)
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