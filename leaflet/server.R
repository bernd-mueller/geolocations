shinyServer(function (input, output) {
  output$map <- renderLeaflet({
    brd
  })
})