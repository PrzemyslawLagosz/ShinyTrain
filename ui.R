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
    fluidRow(
      column(width= 4,
             wellPanel(
               intervalInput("interval"))
             ),
      column(width = 5,
             wellPanel(
               selectNumercInput("select_variable"))
             ),
      column(width = 3,
             wellPanel(
               meanMedianiInput("meanMedian"))
      )
      ),
    fluidRow(
      column(width = 12,
             plotUI("interval_plot"))
    )
  )
  
)
  