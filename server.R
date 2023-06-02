server <- function(input, output, session) {
  
  
  
  
  selectNumericColumnServer("select_variable")
  c(y_axis, column_interval) %<-% selectNumericColumnServer("select_variable")
  
  intervalServer("interval", currentIntervalColumn = column_interval)
  interval_value_selected <- intervalServer("interval", currentIntervalColumn = column_interval)
  
  
  meanMedianiServer("meanMedian")
  mean_median_choice <- meanMedianiServer("meanMedian")
  
    plotServer("interval_plot",
               column_interval = column_interval, 
               y_axis = y_axis, 
               interval_value = interval_value_selected, 
               mean_median = mean_median_choice,
               filterSlider_vals = filterSlider_vals)

  filterSliderServer("filterSlider", currentIntervalColumn = column_interval)
  filterSlider_vals <- filterSliderServer("filterSlider", currentIntervalColumn = column_interval)
}