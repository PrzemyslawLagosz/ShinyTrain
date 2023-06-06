library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)

source("modules.R")

ui <- dashboardPage(
  dashboardHeader(
    title = "Train"
  ),
  dashboardSidebar(disable = TRUE, collapsed = TRUE),
  dashboardBody(
    useShinyjs(), ########### debounce to do ########
    fluidRow(
      column(width= 6,
             wellPanel(
               style = "height: 150px; padding-left: 30px; padding-right: 30px;",
               intervalInput("interval")
               )
             ),
      column(width = 4,
             wellPanel(
               #style = "height: 150px; overflow-y: auto;",
               selectNumericColumnInput("select_variable")
               )
             ),
      column(width = 2,
             wellPanel(
               style = "height: 150px; overflow-y: auto;",
               meanMedianiInput("meanMedian")
               )
      )
      ),
    fluidRow(
      column(width = 12,
             wellPanel(
               style = "height: 740px; overflow-y: auto;",
               plotUI("interval_plot")
               )
             )
    ),
    fluidRow(
      column(width = 12,
             wellPanel(
              filterSliderUI("filterSlider")
             )
               
      )
    )
  )
  
)
  