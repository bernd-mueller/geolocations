shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(
    # Give the page a title
    titlePanel("Bestellungen 2020, Europäische Union"),
    mainPanel(leafletOutput("map"))
  )
)