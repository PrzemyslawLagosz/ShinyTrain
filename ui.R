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
      column(width= 3,
             intervalInput("interval")),
      column(width = 8,
             selectNumercInput("select_variable")
      )),
    fluidRow(
      column(width = 12,
             plotUI("interval_plot"))
    )
  )
  
)
  