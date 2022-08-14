
# Taller de visualización de datos laborales ------------------------------

rm(list = ls())
# Cargar paquetes ---------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse,
               sjPlot,
               stringr)

# Cargar datos ------------------------------------------------------------
data <- readRDS("output/proc.rds")


# Graficar ----------------------------------------------------------------


## Gráfico de barras -------------------------------------------------------
### Básico
barra <- data %>% 
  group_by(iso3c) %>% 
  filter(!is.na(Govint_ictwss)) %>% 
  ggplot(aes(x = Govint_ictwss)) + 
  geom_bar(color = "black", fill = "red")
barra

### Giro de eje
barra <- barra +
  coord_flip()
barra

### Personalizar texto eje X
barra <- barra +
  scale_x_discrete(labels = function(Govint_ictwss) str_wrap(Govint_ictwss, 
                                                             width = 30))
barra

### Títulos
barra <- barra + 
  labs(title="Government intervention in 
wage bargaining",
     x ="", y = "Total")
barra

### Tema 
barra + theme_minimal()
barra

save_plot("output/fig/barras.jpg", fig = last_plot())
#save_plot("output/fig/imagen.jpg", fig = barra)

# Histograma --------------------------------------------------------------


