source("helpers.R")

### INTERVAL SELECTOR ####
intervalInput <- function(id) {
  ns <- NS(id)
  
  sliderInput(ns("interval_input"), "Select interval", min = 1, max = 100000, value = 5,  step = 1)
}

intervalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$interval_input, {
      message(input$interval_input)
    })
    
    return(reactive(input$interval_input))
    
  })
}

#### PLOT ####


plotUI <- function(id) {
  ns <- NS(id)
  
  plotOutput(ns("main_plot"))
}

plotServer <- function(id, column_interval, y_axis, interval_value, mean_median) {
  moduleServer(id, function(input, output, session) {
    
    interval_df <- reactive(interval_group_by_2(auta, 
                                                column_interval = column_interval(), 
                                                column_calculate =  y_axis(), 
                                                interval = interval_value()))
    
    output$main_plot <- renderPlot({
      if (mean_median() == "mean") {
        
      ggplot(interval_df(), aes(x = reorder(year_intervals , year_intervals), y= mean_by_interval /1000)) +
        geom_col() +
        labs(
          title = sprintf("Mean %s", y_axis()),
          subtitle = sprintf("Interval %s years", interval_value()),
          x = NULL,
          y = sprintf("%s", y_axis())
        ) +
        scale_y_continuous(labels = scales::number_format(suffix = " k")) + 
        theme(legend.position="none",
              axis.text.x = element_text(angle = 45, vjust = 0.5)) +
        geom_hline(yintercept = mean(interval_df()$mean_by_interval )/1000,
                   linetype='dashed')+
        annotate(geom = "text",
                 label = sprintf("Mean %s", y_axis()),
                 y = mean(interval_df()$mean_by_interval )/1000,
                 x = nrow(interval_df())-1,
                 vjust = -1)
        
      } else if (mean_median() == "median") {
        
        ggplot(interval_df(), aes(x = reorder(year_intervals , year_intervals), y= median_by_interval /1000)) +
          geom_col() +
          labs(
            title = sprintf("Median %s", y_axis()),
            subtitle = sprintf("Interval %s years", interval_value()),
            x = NULL,
            y = sprintf("%s", y_axis())
          ) +
          scale_y_continuous(labels = scales::number_format(suffix = " k")) + 
          theme(legend.position="none",
                axis.text.x = element_text(angle = 45, vjust = 0.5)) +
          geom_hline(yintercept = mean(interval_df()$median_by_interval )/1000,
                     linetype='dashed')+
          annotate(geom = "text",
                   label = sprintf("Median %s", y_axis()),
                   y = mean(interval_df()$median_by_interval )/1000,
                   x = nrow(interval_df())-1,
                   vjust = -1)
        
      }
      
    }, res = 96, height = 800)
  })
}

#### SELECT Y_AXIS ON PLOT AND COLUMN TO INTERVAL DATA FRAME ####
selectNumercInput <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(width = 3,
           selectInput(ns("interval_column_select"), "Choose interval column", choices = numericColnames, selected = "Rok.produkcji")),
    column(width = 3,
           selectInput(ns("y_axis_select"), "Choose Y axis column", choices = numericColnames, selected = "Cena.w.PLN"))
  )

}

selectNumerServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$y_axis_select, {
      message(input$y_axis_select)
      message(typeof(input$y_axis_select))
    })
    
    list(
      reactive(input$y_axis_select),
      reactive(input$interval_column_select)
    )
    
  })
}

####

meanMedianiInput <- function(id) {
  ns <- NS(id)
  
  choices = c("mean", "median")
  
  radioButtons(ns("radio"), label = "What to calculate", choices = choices)
}


meanMedianiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$radio, {
      message(input$radio)
      message(typeof(input$radio))
    })
    
    return( reactive(input$radio) )
  })
}