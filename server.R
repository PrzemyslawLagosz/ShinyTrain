server <- function(input, output, session) {
  intervalServer("interval")
  interval_selected <- intervalServer("interval")
  
  plotServer("interval_plot", interval_value = interval_selected)
  
  selectNumerServer("select_variable")
}