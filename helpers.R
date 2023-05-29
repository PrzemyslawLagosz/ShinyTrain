library(tidyverse)
library(ggplot2)
library(shiny)
library(rlang)

setwd("C:/Users/Przemo/Desktop/IT/R/TrainShiny01")
load("auta.Rdata")

seq_last <- function(x, interval) {
  #x is vector
  seq <- seq(from = min(x), to = max(x), by = interval)
  
  if (tail(seq, 1) == max(x)) {
    return(seq)
  } else {
    seq <- c(seq, max(x))
    return(seq)
  }
}

############                     SUPER FAJNE ROZWIAZANIE JAK PODAWAC ARGUMENTY DO FUNKCJI GDZIE UZYWAMY GROUPBY ITP ################

interval_group_by <- function(df, column_interval, column_calculate, interval, dig.lab = 5) {
  # DF to coalculeta
  # column_interval <- column turned into intervals with cut and group_by
  # column_calcularte <- column to calculate mean and median in intervals
  # interval <- value of the interval
  
  column_interval <- enquo(column_interval)
  column_calculate <- enquo(column_calculate)
  
  print(column_interval)
  
  df_grouped_by_interval <- df %>%
    mutate(year_intervals = cut(!!column_interval, seq_last(!!column_interval, interval), dig.lab = dig.lab)) %>% 
    group_by(year_intervals) %>%
    summarise(mean_by_interval = mean(!!column_calculate),
              median_by_interval = median(!!column_calculate)) %>%
    drop_na()
  
  return(df_grouped_by_interval)
}

interval_df <- interval_group_by(auta, Rok.produkcji, Cena.w.PLN, 13)
interval_df

interval_group_by(auta, Rok.produkcji, Cena.w.PLN, 13)
#################

interval_group_by_2 <- function(df, column_interval, column_calculate, interval, dig.lab = 5) {
  # DF to coalculeta
  # column_interval <- column turned into intervals with cut and group_by
  # column_calcularte <- column to calculate mean and median in intervals
  # interval <- value of the interval
  
  df_grouped_by_interval <- df %>%
    mutate(year_intervals = cut(!! sym(column_interval), seq_last(!! sym(column_interval), interval), dig.lab = dig.lab)) %>% 
    group_by(year_intervals) %>%
    summarise(mean_by_interval = mean(!! sym(column_calculate)),
              median_by_interval = median(!! sym(column_calculate))) %>%
    drop_na()
  
  return(df_grouped_by_interval)
}

interval_group_by_2(auta, "Cena", "Cena.w.PLN", 13)

######

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


numericColnames <- colnames(select_if(auta, is.numeric))
