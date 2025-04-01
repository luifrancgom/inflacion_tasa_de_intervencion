# Libraries ----
library(tidyverse)
library(tidymodels)
library(latex2exp)
library(kableExtra)

# Import data ---- 
pi_t_i_t <- read_csv(file = "clean_data/inflacion_anual_tasa_de_interes_de_politica_monetaria.csv")

# Delete NA values
pi_t_i_t <- pi_t_i_t |> 
  drop_na()

# Linear regression
lm_model <- pi_t_i_t |> 
  lm(formula = interest_rate_t ~ inflation_t, data = _) |> 
  tidy()

lm_model_table <- lm_model |>
  set_names(nm = c("Término", "Estimación", 
                   "Error estandar", "Estadístico",
                   "P-valor")) |> 
  mutate(Término = c("$\\beta_0$", "$\\beta_1$")) |> 
  kbl(format = "html", escape = FALSE, 
      digits = 3) |> 
  kable_styling(latex_options = "striped")

# Visualization ----
gplot <- pi_t_i_t |>
  ggplot(aes(inflation_t, interest_rate_t)) +
  geom_point(shape = 21,
             color = "black",
             fill  = "#E31A1C") + 
  geom_smooth(method = "lm", 
              se = FALSE,
              color = "#2C3E50") +
  geom_text(x = 5, y = 25, 
            label = TeX(r'($i_t = -0.762 + 1.409 \pi_t$)'), 
            parse = TRUE) +
  labs(x = TeX(r'(Inflación anual ($\pi_t$))'),
       y = TeX(r'(Tasa de interés de política monetaria ($i_t$))'),
       title = "Tasa de interés de política monetaria vs Inflación anual para Colombia",
       subtitle = str_glue("Periodo: {first(pi_t_i_t$date)} - {last(pi_t_i_t$date)}
                           Observación: para ajustar la frecuencia diaria a mensual de la tasa de interés de política 
                           se utiliza el promedio mensual dado que la tasa de inflación anual tiene una frecuencia 
                           mensual"),
       caption = str_glue("Fuente datos: Portal de estadísticas económicas - BanRep
                          Fecha de corte: 2025-04-01"))