
# Taller de visualización de datos laborales ------------------------------

rm(list = ls())
# Cargar paquetes ---------------------------------------------------------
#install.packages("pacman")
pacman::p_load(tidyverse,
               sjPlot,
               stringr,
               car)

# Cargar datos ------------------------------------------------------------
data <- readRDS("output/proc.rds")


# Graficar ----------------------------------------------------------------


# Univariado --------------------------------------------------------------

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
  labs(title="Intervención gubernamental en negociaciones 
salariales",
     x ="", y = "Total",
     caption = "Elaboración propia en base a ICTWSS")
barra

### Tema 
barra + theme_minimal()
barra

save_plot("output/fig/barras.jpg", fig = barra)
#save_plot("output/fig/imagen.jpg", fig = last_plot())

## Histograma --------------------------------------------------------------
histo <- data %>% 
  group_by(iso3c, year) %>% 
  filter(!is.na(db_wdi)) %>% 
  ggplot(aes(x = db_wdi)) + 
  geom_histogram(color = "black", fill = "red") + 
  labs(title="Ease of Doing Business Index",
       x ="", y = "Total",
       caption = "Elaboración propia en base a WDI") +
  theme_minimal()
histo

save_plot("output/fig/histo.jpg", fig = histo)

# Bivariado ---------------------------------------------------------------

## Puntos bivariado --------------------------------------------------------
bi_puntos <- data %>% 
  group_by(Govint_ictwss) %>% 
  summarise(mrw = mean(mrw_oecd, na.rm = T)) %>% 
  filter(!is.na(Govint_ictwss)) %>% 
  ggplot(aes(x = Govint_ictwss,
             y = mrw, 
             color = str_wrap(Govint_ictwss,
                                      width = 30))) + 
  geom_point() +
  labs(title="Relación entre intervención gubernamental en negociaciones 
salariales y salarios mínimos en razon de salarios medianos 
para trabajadores a tiempo completo",
       x ="", y = "Salarios mínimos en razon de salarios medianos 
para trabajadores a tiempo completo",
       caption = "Elaboración propia en base a V-Dem y OECD",
       color = "Intervención gubernamental en negociaciones 
salariales") +
  theme(axis.text.x = element_blank(),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        plot.caption = element_text(hjust = -2)) 
bi_puntos

save_plot("output/fig/bi_puntos.jpg", fig = bi_puntos)

## Dispersión --------------------------------------------------------------
scatter <- data %>% 
  group_by(iso3c, year) %>% 
  filter(!is.na(ud_ilo_stat) & !is.na(mrw_oecd)) %>% 
  ggplot(aes(x = ud_ilo_stat, y = mrw_oecd)) + 
  geom_point(color = "red") + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(title="Relación entre densidad sindical y salarios 
mínimos en razon de salarios medianos 
para trabajadores a tiempo completo",
       x ="Densidad sindical", y = "Salarios mínimos en razon de salarios medianos 
para trabajadores a tiempo completo",
       caption = "Elaboración propia en base a ILO-Stat y OECD") +
  theme_minimal() 
scatter

save_plot("output/fig/scatter.jpg", fig = scatter)

# Longitudinal ------------------------------------------------------------


## Univariado --------------------------------------------------------------

long_u <- data %>% 
  filter(year >= 1990 & !is.na(women_empower_vdem)) %>% 
  group_by(year) %>% 
  summarise(wei = mean(women_empower_vdem, na.rm = T)) %>% 
  ggplot(aes(x = year, y = wei)) + 
  geom_line(color = "purple") +
  labs(title="Transformaciones en el Índice de 
Empoderamiento Político Femenino (1990-2020)",
       x ="Año", y = "Índice de 
Empoderamiento Político Femenino",
       caption = "Elaboración propia en base a V-Dem") +
  theme_minimal() 
long_u
save_plot("output/fig/long_u.jpg", fig = long_u)


## Bivariado ---------------------------------------------------------------

long_b <- data %>% 
  filter(year >= 1990 & 
           !is.na(women_empower_vdem) & 
           !is.na(hourearn_fem_isco08_total_ilo_stat)) %>% 
  group_by(year) %>% 
  summarise(cor = cor(women_empower_vdem, 
                      hourearn_fem_isco08_total_ilo_stat,
                      use = "pairwise.complete.obs",
                      method = "pearson")) %>% 
  ggplot(aes(x = year, y = cor)) + 
  geom_line(color = "purple") +
  labs(title="Transformaciones en la relación entre el
Índice de Empoderamiento Político Femenino y 
el salario femenino medio por hora (1990-2020)",
       x ="Año", y = "",
       caption = "Elaboración propia en base a V-Dem e ILO-Stat") +
  theme_minimal() 
long_b
save_plot("output/fig/long_b.jpg", fig = long_b)


## Incorporar países -------------------------------------------------------

ud_pais <- data %>% 
  filter(year >= 2000 & 
           iso3c %in% c("CHL", "ARG", "USA", "DEU") &
           !is.na(ud_ilo_stat)) %>% 
  group_by(iso3c, year) %>% 
  ggplot(aes(x = year, y = ud_ilo_stat, color = iso3c)) + 
  geom_line() +
  labs(title="Transformaciones en la densidad sindical (2000-2020)",
       x ="Año", y = "Densidad sindical (%)",
       color = "Países",
       caption = "Elaboración propia en base a V-Dem e ILO-Stat") +
  scale_color_discrete(labels=c('Argentina', 'Chile',
                               'Alemania', 'Estados Unidos')) +
  theme_minimal() 
ud_pais
save_plot("output/fig/ud_pais.jpg", fig = ud_pais)

