source("helpers.R")

intervalInput <- function(id) {
  ns <- NS(id)
  
  sliderInput(ns("interval_input"), "Select interval", min = 1, max = 10, value = 5,  step = 1)
}

intervalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$interval_input, {
      message(input$interval_input)
    })
    
    interval_df <- interval_group_by(auta, Rok.produkcji, Cena.w.PLN, 13)
    
    return(reactive(input$interval_input))
    
  })
}

plotUI <- function(id) {
  ns <- NS(id)
  
  plotOutput(ns("main_plot"))
}

plotServer <- function(id, interval_value) {
  moduleServer(id, function(input, output, session) {
    
    interval_df <- reactive(interval_group_by(auta, Rok.produkcji, Cena.w.PLN, interval = interval_value()))
    
    output$main_plot <- renderPlot({
      
      ggplot(interval_df(), aes(x = reorder(year_intervals , year_intervals), y= mean_by_interval /1000)) +
        geom_col() +
        labs(
          title = "Mean price",
          subtitle = sprintf("Interval %s years", interval_value()),
          x = NULL,
          y = "Price"
        ) +
        scale_y_continuous(labels = scales::number_format(suffix = " k")) + 
        theme(legend.position="none",
              axis.text.x = element_text(angle = 45, vjust = 0.5)) +
        geom_hline(yintercept = mean(interval_df()$mean_by_interval )/1000,
                   linetype='dashed')+
        annotate(geom = "text",
                 label = "Mean price",
                 y = mean(interval_df()$mean_by_interval )/1000,
                 x = nrow(interval_df())-1,
                 vjust = -1)
      
    })
  })
}

selectNumercInput <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(width = 3,
           selectInput(ns("interval_select"), "Choose interval column", choices = numericColnames)),
    column(width = 3,
           selectInput(ns("y_axis_select"), "Choose Y axis column", choices = numericColnames))
  )

}

selectNumerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$y_axis_select, {
      message(input$y_axis_select)
      message(typeof(input$y_axis_select))
    })
    
    return(reactive(input$y_axis_select))
    
  })
}