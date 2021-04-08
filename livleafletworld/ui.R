shinyUI(
  # Use a fluid Bootstrap layout
  fluidPage(
    # Give the page a title
    titlePanel("Benutzer 2020, Global"),
    mainPanel(leafletOutput("map"))
  )
)