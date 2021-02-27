shinyServer(function (input, output) {
  output$map <- renderLeaflet({
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
    m
  })
})