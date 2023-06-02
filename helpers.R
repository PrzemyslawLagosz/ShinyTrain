library(tidyverse)
library(ggplot2)
library(shiny)
library(rlang)
library(zeallot)
library(tidyr)

setwd("C:/Users/Przemo/Desktop/IT/R/TrainShiny01")
load("auta.Rdata")

# Removing outliners
mask1 <- quantile(auta$Przebieg.w.km, probs = 0.9995, na.rm= TRUE) 

auta <- auta %>%
  filter(Przebieg.w.km < mask1 & KM < 1100 & Pojemnosc.skokowa < 18000)


seq_last <- function(x, interval) {
  #' @param x is vector
  #' 
  #' @return sequence like seq() function, but include laste not full interval
  
  seq <- seq(from = min(x), to = max(x), by = interval)
  
  if (tail(seq, 1) == max(x)) {
    return(seq)
  } else {
    seq <- c(seq, max(x))
    return(seq)
  }
}


# Columns used to filter main plot in APP
numericColnames <- colnames(select_if(auta, is.numeric))

#, dig.lab = 50 w definiciji interval_group i potem w mutate
interval_group_by_2 <- function(df, column_interval, column_calculate, interval, range_vals = c(1950, 2000), dig.lab = 50) {
  #' @param DF to calculate
  #' @param column_interval <- column turned into intervals with cut and group_by
  #' @param column_calculate <- column to calculate mean and median in intervals
  #' @param interval <- value of the interval
  #' @return DATA FRAM grouped by @param column_interval and calculated mean, median for @param column_calculate
  
  df_grouped_by_interval <- df %>%
    filter(!! sym(column_interval) >= range_vals[1] & !! sym(column_interval) <= range_vals[2]) %>% 
    drop_na() %>% 
    mutate(year_intervals = cut(!! sym(column_interval), seq_last(!! sym(column_interval), interval), dig.lab = dig.lab)) %>% 
    group_by(year_intervals) %>%
    summarise(mean_by_interval = mean(!! sym(column_calculate), na.rm = TRUE),
              median_by_interval = median(!! sym(column_calculate)),
              n = n()) %>% 
    drop_na()
  
  return(df_grouped_by_interval)
}

interval_df <- interval_group_by_2(auta, "Rok.produkcji", "Cena", 13)

### Prototype PLOT without reactive inputs

ggplot(interval_df, aes(x = reorder(year_intervals , year_intervals), y= mean_by_interval /1000)) +
  geom_col() +
  labs(
    title = "Mean price",
    subtitle = "Interval 10 years",
    x = NULL,
    y = "Price"
  ) +
  scale_y_continuous(labels = scales::number_format(suffix = " k")) + 
  theme(legend.position="none",
        axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  geom_hline(yintercept = mean(interval_df$mean_by_interval )/1000,
             linetype='dashed')+
  annotate(geom = "text",
           label = "Mean price",
           y = mean(interval_df$mean_by_interval )/1000,
           x = nrow(interval_df)-1,
           vjust = -1)
 
ggplot(mtcars, aes(x = cyl)) +
  geom_histogram(bins = 3)


interval_input_values <- function(interval_variable) {
  switch(interval_variable,
         "Cena" = list(min = 10e+3, max = 10e+5, step = 10e+3, value = 5*10e+4),
         "Cena.w.PLN" = list(min = 10e+3, max = 10e+5, step = 10e+3, value = 5*10e+4),
         "KM" = list(min = 10, max = 100, step = 10, value = 50),
         "kW" = list(min = 10, max = 100, step = 10, value = 50),
         "Pojemnosc.skokowa" = list(min = 100, max = 1000, step = 100, value = 500),
         "Przebieg.w.km" = list(min = 10e+3, max = 10e+5, step = 10e+3, value = 5*10e+4),
         "Rok.produkcji" = list(min = 1, max = 10, step = 1, value = 5),
  )
}

filter_var <- function(current_range) {
  
  
}

range(auta[["Cena"]])



