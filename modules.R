source("helpers.R")

### INTERVAL SELECTOR ####
intervalInput <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    sliderInput(ns("interval_input"), "Select interval", min = 1, max = 100000, value = 5,  step = 1)
  )
}

intervalServer <- function(id, currentIntervalColumn) {
  
  moduleServer(id, function(input, output, session) {
    
    selectorVals <- reactive( interval_input_values(currentIntervalColumn()) )
    
    observeEvent(currentIntervalColumn(), {
      freezeReactiveValue(input, "interval_input")
      updateSliderInput(session, 
                        inputId = "interval_input", 
                        min = selectorVals()$min, 
                        max = selectorVals()$max, 
                        step = selectorVals()$step, 
                        value = selectorVals()$value)
      
      updateSliderInput(session, "interval_input", label = sprintf("Select interval of %s", currentIntervalColumn()))
    
      
      })
    
    
    return(reactive(input$interval_input))
    
  })
}

#### SELECT Y_AXIS ON PLOT AND COLUMN TO INTERVAL DATA FRAME ####
selectNumericColumnInput <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(width = 3,
           selectInput(ns("interval_column_select"), "Choose interval column", choices = numericColnames, selected = "Rok.produkcji")),
    column(width = 3,
           selectInput(ns("y_axis_select"), "Choose Y axis column", choices = numericColnames, selected = "Cena.w.PLN"))
  )

}

selectNumericColumnServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    list(
      reactive(input$y_axis_select),
      reactive(input$interval_column_select)
    )
    
  })
}

####



filterSliderUI <- function(id) {
  ns <- NS(id)
  
  sliderInput(ns("filterSlider"), "Filter range on X axis to zoom in/out", min = 1900, max = 2012, step = 1, value = c(1900, 2012))
  
}
filterSliderServer <- function(id, currentIntervalColumn) {
  moduleServer(id, function(input, output, session) {
    
    current_range <- reactive(range(auta[[currentIntervalColumn()]]))
    
    observeEvent(currentIntervalColumn(), {
      updateSliderInput(session, 
                        inputId = "filterSlider", 
                        min = current_range()[1],
                        max = current_range()[2],
                        step = 1,
                        value = current_range())
    })
  
    
    return(reactive(input$filterSlider))
    
})
}



#### PLOT ####


plotUI <- function(id) {
  ns <- NS(id)
  
  plotOutput(ns("main_plot"))
}

plotServer <- function(id, column_interval, y_axis, interval_value, filterSlider_vals, mean_median) {
  moduleServer(id, function(input, output, session) {
    
    
    interval_df <- reactive(interval_group_by_2(auta, 
                                                column_interval = column_interval(), 
                                                column_calculate =  y_axis(), 
                                                interval = interval_value(),
                                                range_vals = filterSlider_vals()))
    output$main_plot <- renderPlot({
      if (mean_median() == "mean") {

      ggplot(interval_df(), aes(x = reorder(year_intervals , year_intervals), y= mean_by_interval /1000, fill = n)) +
        geom_col() +
        geom_text(aes(label = n), nudge_y = 5, check_overlap = TRUE) +
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

        ggplot(interval_df(), aes(x = reorder(year_intervals , year_intervals), y= median_by_interval /1000, fill = n)) +
          geom_col() +
          geom_text(aes(label = n), nudge_y = 5, check_overlap = TRUE) +
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
      
    }, res = 96, height = 650)
    
  })
}


meanMedianiInput <- function(id) {
  ns <- NS(id)
  
  choices = c("mean", "median")
  
  radioButtons(ns("radio"), label = "What to calculate", choices = choices)
}


meanMedianiServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # observeEvent(input$radio, {
    #   message(input$radio)
    #   message(typeof(input$radio))
    # })
    
    return( reactive(input$radio) )
  })
}