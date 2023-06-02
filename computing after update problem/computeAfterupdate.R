library(shiny)
ui = shiny::fluidPage(
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::radioButtons("type", "Type", choices = c("5", "15", "21")),
      shiny::sliderInput("x", "x", min = 1, max = 10, value = 7),
      shiny::checkboxInput("auto","Set y to 8 when type is 15, and 9 when type is 21", value = TRUE),
      shiny::sliderInput("y", "y", min = 1, max = 10, value = 7)
    ),
    
    shiny::mainPanel(
      shiny::plotOutput("show")
    )
  )
)
server = function(input, output, session) {
  plotData = shiny::reactive({
    list(x = input$x, y = input$y, type = input$type)
  })
  
  output$show = renderPlot({
    shiny::req(plotData())
    d = plotData()
    print("refresh")
    plot(d$x,d$y, pch = as.integer(d$type))
  })
  
  
  observe({
    n = input$n
    a = input$auto
    type = input$type
    
    if (type == "a") return(NULL)
    if (a) shiny::updateSliderInput(session, "y", value = switch(type, "15" = 8, "21" = 9))
  })
}
shiny::shinyApp(ui = ui, server = server)