shinyServer(function (input, output) {
  output$map <- renderLeaflet({
    livworld
  })
})