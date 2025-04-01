# Libraries ----
library(tidyverse)
library(timetk)
library(janitor)

# Import data ----
## Variable: Annual inflation
## Frequency: Monthly
## Source: Portal de Estadísticas Económicas - Banrep
### https://suameca.banrep.gov.co/estadisticas-economicas/#/home
pi_t <- read_csv(file = "raw_data/inflacion_y_meta.csv") 
## Variable: Monetary policy interest rate
## Frequency: Daily
## Source: Portal de Estadísticas Económicas - Banrep
## https://suameca.banrep.gov.co/estadisticas-economicas/#/home
i_t  <- read_csv(file = "raw_data/tasa_de_interes_de_politica_monetaria.csv")

# Clean data ---- 
pi_t <- pi_t |> 
  clean_names() |> 
  set_names(nm = c("date", "target", "inflation_t")) |> 
  select(date, inflation_t)

i_t <- i_t |> 
  clean_names()  |> 
  set_names(nm = c("date", "interest_rate_t"))

# Aggregating ---
## To adjust to monthly frequency using the monthly mean
i_t_monthly <- i_t |> 
  summarize_by_time(.date_var = date, 
                    .by = "month", 
                    interest_rate_t = mean(interest_rate_t),
                    .type = "ceiling") |> 
  mutate(date = date %-time% "1 day")

# Merge ----
i_t_pi_t <- i_t_monthly |> 
  left_join(y = pi_t, 
            by = join_by(date))

# Export ---
i_t_pi_t |> 
   write_csv(file = "clean_data/inflacion_anual_tasa_de_interes_de_politica_monetaria.csv")

# Check data ---
read_csv(file = "clean_data/inflacion_anual_tasa_de_interes_de_politica_monetaria.csv")