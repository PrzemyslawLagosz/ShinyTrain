server <- function(input, output, session) {
  
  intervalServer("interval")
  interval_value_selected <- intervalServer("interval") # Step to 
  
  selectNumerServer("select_variable")
  
  meanMedianiServer("meanMedian")
  mean_median_choice <- meanMedianiServer("meanMedian")
  
  c(y_axis, column_interval) %<-% selectNumerServer("select_variable")
  
  plotServer("interval_plot", column_interval = column_interval, y_axis = y_axis, interval_value = interval_value_selected, mean_median = mean_median_choice)
}