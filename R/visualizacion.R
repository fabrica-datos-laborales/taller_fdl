
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
  labs(title="Government intervention in 
wage bargaining",
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
  labs(title="Relationship between Government intervention 
in wage setting and Minimum relative to median 
wages of full-time workers",
       x ="", y = "Minimum relative to median 
wages of full-time workers",
       caption = "Elaboración propia en base a V-Dem y OECD",
       color = "Government intervention in 
wage setting") +
  theme(axis.text.x = element_blank(),
        panel.background = element_rect("white"),
        panel.grid = element_line("grey80"),
        plot.caption = element_text(hjust = -2)) 
bi_puntos

save_plot("output/fig/bi_puntos.jpg", fig = bi_puntos)

## Dispersión --------------------------------------------------------------
scatter <- data %>% 
  group_by(iso3c, year) %>% 
  filter(!is.na(`ud_ilo-stat`) & !is.na(mrw_oecd)) %>% 
  ggplot(aes(x = `ud_ilo-stat`, y = mrw_oecd)) + 
  geom_point(color = "red") + 
  geom_smooth(method = "lm", colour = "black") + 
  labs(title="Relationship between Trade union density and 
Minimum relative to median wages of full-time 
workers",
       x ="Trade union density", y = "Minimum relative to median 
wages of full-time workers",
       caption = "Elaboración propia en base a ILO-Stat y OECD") +
  theme_minimal() 
scatter

save_plot("output/fig/scatter.jpg", fig = scatter)

# Longitudinal ------------------------------------------------------------


## Univariado --------------------------------------------------------------

long_u <- data %>% 
  filter(year >= 1990 & !is.na(women_empower_vdem_vdem)) %>% 
  group_by(year) %>% 
  summarise(wei = mean(women_empower_vdem_vdem, na.rm = T)) %>% 
  ggplot(aes(x = year, y = wei)) + 
  geom_line(color = "purple") +
  labs(title="Evolution of Women Political Empowerment Index 
(1990-2020)",
       x ="Year", y = "Women Political Empowerment Index",
       caption = "Elaboración propia en base a V-Dem") +
  theme_minimal() 
long_u
save_plot("output/fig/long_u.jpg", fig = long_u)


## Bivariado ---------------------------------------------------------------

long_b <- data %>% 
  filter(year >= 1990 & 
           !is.na(women_empower_vdem_vdem) & 
           !is.na(`hourearn_fem_isco08_total_ilo-stat`)) %>% 
  group_by(year) %>% 
  summarise(cor = cor(women_empower_vdem_vdem, 
                      `hourearn_fem_isco08_total_ilo-stat`,
                      use = "pairwise.complete.obs",
                      method = "pearson")) %>% 
  ggplot(aes(x = year, y = cor)) + 
  geom_line(color = "purple") +
  labs(title="Evolution of the relationship
between Women Political Empowerment Index and
Female hourly salary (1990-2020)",
       x ="Year", y = "",
       caption = "Elaboración propia en base a V-Dem e ILO-Stat") +
  theme_minimal() 
long_b
save_plot("output/fig/long_b.jpg", fig = long_b)

