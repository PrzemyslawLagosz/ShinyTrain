library(tidyverse)
library(ggplot2)
library(shiny)
library(rlang)
library(zeallot)
library(tidyr)

setwd("C:/Users/Przemo/Desktop/IT/R/TrainShiny01")
load("auta.Rdata")

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

x <- interval_group_by_2(auta, "Cena.w.PLN", "Przebieg.w.km", 100000)

numericColnames

tmp <- unlist(map(auta[numericColnames], function(x) max(x, na.rm = T)-min(x, na.rm = T)))

tmp
# Cena        Cena.w.PLN                KM                kW Pojemnosc.skokowa     Przebieg.w.km     Rok.produkcji

interval_group_by_2(auta, "Cena.w.PLN", "Przebieg.w.km", 100000, dig.lab = 10)

interval_group_by_2(auta, "KM", "Przebieg.w.km", 100, dig.lab = 10)

interval_group_by_2(auta, "Przebieg.w.km", "Cena.w.PLN", 100000, dig.lab = 10)

quantile(auta$Przebieg.w.km, probs = 0.999, na.rm= TRUE)


table(auta$Przebieg.w.km > quantile(auta$Przebieg.w.km, probs = 0.999, na.rm= TRUE))

quantile(auta$Przebieg.w.km, probs = 0.999, na.rm= TRUE)

mask1 <- quantile(auta$Przebieg.w.km, probs = 0.9995, na.rm= TRUE) 
mask2 <- quantile(auta$Pojemnosc.skokowa, probs = 0.9995, na.rm= TRUE)

tmp2 <- auta %>%
  filter(Przebieg.w.km > mask1 | KM > 1100 | Pojemnosc.skokowa > 18000)

auta2 <- auta %>%
  filter(Przebieg.w.km < mask1 & KM < 1100 & Pojemnosc.skokowa < 18000)

tmp3 <- unlist(map(auta2[numericColnames], function(x) max(x, na.rm = T)-min(x, na.rm = T)))

tmp3

interval_group_by_2(auta2, "Cena.w.PLN", "Przebieg.w.km", 100000, dig.lab = 10)

interval_group_by_2(auta2, "KM", "Przebieg.w.km", 100, dig.lab = 10)

interval_group_by_2(auta2, "Przebieg.w.km", "Cena.w.PLN", 10e+4, dig.lab = 10)

interval_group_by_2(auta2, "Pojemnosc.skokowa", "Cena.w.PLN", 100, dig.lab = 10)

tmp3

vars <- tibble::tribble(
  ~ id,   ~ min, ~ max,
  "Cena",     0,     1,
  "beta",      0,    10,
  "gamma",    -1,     1,
  "delta",     0,     1,
)

interval_input_values <- function(interval_variable) {
  switch(interval_variable,
         "Cena" = list(min = 10e+3, max = 10e+5, step = 10e+3),
         "Cena.w.PLN" = list(min = 10e+3, max = 10e+5, step = 10e+3),
         "KM" = list(min = 10, max = 100, step = 10),
         "kW" = list(min = 10, max = 100, step = 10),
         "Pojemnosc.skokowa" = list(min = 100, max = 1000, step = 100),
         "Przebieg.w.km" = list(min = 10e+3, max = 10e+5, step = 10e+3),
         "Rok.produkcji" = list(min = 1, max = 10, step = 1),
         )
}


temp <- xdd("Cena")

temp$min
