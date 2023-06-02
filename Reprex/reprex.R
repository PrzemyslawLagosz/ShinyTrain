library(shiny)
library(shinydashboard)
library(zeallot)


selectUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    selectInput(ns("selector"), "Selectro", choices = c("big", "small")),
    selectInput(ns("selector2"), "Selectro2", choices = c("big2", "small2"))
  )
}

selectServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    list(
      reactive(input$selector),
      reactive(input$selector2)
    )
    
  })
}

sliderUI <- function(id) {
  ns <- NS(id)
  
  sliderInput(ns("slider"), "Slider", min = 1, max = 10, value = 5, step = 1)
}

sliderServer <- function(id, selectorVal) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(selectorVal(), {
      if (selectorVal() == "big") {
        updateSliderInput(session, inputId = "slider", min = 1, max = 100, value = 50, step = 10)
      } else if (selectorVal() == "small") {
        updateSliderInput(session, inputId = "slider", min = 1, max = 10, value = 5, step = 1)
      }
    })
    
    
    
    })
}


ui <- dashboardPage(
  dashboardHeader(
    title = "Reprex"
  ),
  dashboardSidebar(disable = TRUE, collapsed = TRUE),
  dashboardBody(
    fluidRow(
      wellPanel(
        selectUI("selectors")
      )
    ),
    fluidRow(
      wellPanel(
        sliderUI("sliderToUpdate")
      )
    )
  )
  
)

server <- function(input, output, session) {
  
  selectServer("selectors")
  c(selector1_value, selector2_value) %<-% selectServer("selectors")
  
  sliderServer("sliderToUpdate", selectorVal = selector1_value)

}

shinyApp(ui, server)