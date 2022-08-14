
# Taller de visualización de datos laborales ------------------------------

# Cargar paquetes ---------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse,
               sjPlot)

# Cargar datos ------------------------------------------------------------
data <- readRDS("output/proc.rds")


# Graficar ----------------------------------------------------------------


## Gráfico de barras -------------------------------------------------------
### Básico
data %>% 
  filter(!is.na(Govint_ictwss)) %>% 
  ggplot(aes(x = Govint_ictwss)) + 
  geom_bar()

### Giro de eje
data %>% 
  filter(!is.na(Govint_ictwss)) %>%
  ggplot(aes(x = Govint_ictwss)) + 
  geom_bar() +
  coord_flip()
